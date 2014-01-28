#lang racket/base

;; Stream processing DSL surface syntax.
;; See stream.rkt for Racket #lang interface
;;
;; The word "syntax" is context dependent and can refer to:
;;
;; - DSL syntax : abstract syntax (AS) implemented as lambda
;;   expressions parameterized by semantics.
;;
;; - Surface syntax: Scheme macros used for syntactic sugaring.
;;
;; The Scheme macros implement embedding of the stream language in
;; Racket, while the abstract syntax is used to endow code
;; representations with distinct interpretations.
;;
;; While interpretation of DSL syntax is arbitrary, interpretation
;; makes most sense when several distinct interpretations are related,
;; where one interpretation can shed light on properties of another.
;;
;; This is the theory of Abstract Interpretation
;; http://en.wikipedia.org/wiki/Abstract_interpretation
;;
;; The canonical interpretation of the language is in terms of
;; arithmetic primitives lifted over causal streams.
;;

(require racket/stxparam
         "tools.rkt"
         "type.rkt"
         (for-syntax
          racket/base
          "tools.rkt"))

(provide (all-defined-out))

;;; *** Scheme surface syntax ***

;; DSL syntax is represented as lambda abstractions written in terms
;; primitive (or named composite) functions. The meaning of the
;; primitive functions is represented as a struct `ai' present as the
;; first argument in any abstraction `ai-lambda' or application
;; `ai-app'.  These two macros perform the implicit lexical threading
;; of this first argument using the syntax parameter `ai-semantics'.

;; It is crucial to keep the syntax referentially transparent.  The
;; semantics object should be passed lexically, i.e. though a local
;; variable, not through a dynamic or global variables.

;; This is very similar to how type classes are implemented in
;; Haskell.  Essentially, all primitives and composite DSL syntax
;; objects are generic functions.

;; http://docs.racket-lang.org/reference/stxparam.html
(define-syntax-parameter ai-semantics (lambda (stx) #'#f))

(define ai-function-printer
  (make-parameter
   (lambda (fun port mode)
     (when mode (write-string "#<" port))
     (write-string "ai-function " port)
     (write-string (format "~a" (ai-function-args fun)) port)
     (when mode (write-string ">" port)))))

;; All lambda forms are wrapped in this struct together with info that
;; needs to be transported to the next stage.
(define-struct ai-function (proc args)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (lambda args (apply (ai-function-printer) args)))])



(define (ai-function-arity f) (length (ai-function-args f)))

;; The DSL's main lambda form wraps the representation function in a
;; `ai-function' struct and adds an extra argument to the
;; representation function.  This arguments hold the language
;; semantics (a collection of primitive functions) provided by the
;; top-level evaluator.  Note that argument evaluation is lazy.
(define-syntax-rule (ai-lambda
                     (arg ...)
                     expr)
  (make-ai-function
   (lambda (p arg ...)
     (syntax-parameterize
      ((ai-semantics (lambda _ #'p)))
      expr))
   '(arg ...)))

;; The `ai-app' mirrors `ai-lambda'.  It unwraps the ai-function
;; structure layer and passes the semantics struct as first parameter.
(define-syntax-rule (ai-app fn a ...)
  ((ai-function-proc fn)
   (ai-semantics)
   a ...))

;; Return operator.  Currently hardcoded.  Could be abstracted as prim
;; if necessary.
(define ai-values
  (make-ai-function
   (lambda (ai . a)
     (apply values a))
   #f))


;;; *** Surface syntax for causal system specification ***

;; Specification of a causal system using output delay is done using
;; `ai-lambda-feedback' surface syntax, which used the functionality of
;; the abstract `feedback' primitive.

;; Such a function takes a number of state stream inputs, and produces
;; the same number of state stream outputs preceeding its usual
;; outputs.

(define-syntax ai-lambda-feedback
  (syntax-rules ()
    ((_ (((s s0) ...) ;; State inputs + init: fed back internally, hidden from outside.
         (i ...)      ;; Outside inputs.
         (t ...))     ;; Time coordinates: current index, time block size
        expr-update)  ;; Compute state update + outside output.

     ;; Abstract internal code blocks as functions.
     (let ((update (ai-lambda (s ... i ... t ...) expr-update)))
       ;; Abstract everything as a stream processor with state
       ;; variables tucked away behind the scenes.
       (ai-lambda (i ...)
          ;; The rest of the plumbing is done here: state nodes are
          ;; created in the background and threaded through the code,
          ;; time coordinates are bound to global time and setup and
          ;; update are sequenced properly.
          (ai-app ai-feedback/n
             '((s . s0) ...)
             `(,i ...)
             '(t ...)
             update    ;; (s, i, (t T), l) -> (s, o)
             ))))))

(define-syntax ai-feedback
  (syntax-rules ()
    ((_ (s0 ...) fn)
     (ai-app ai-feedback/n
             '((#f . s0) ...)
             #f ;; get arity from fn
             #f ;; get arity
             fn))))
        

;; hold and setup are call-by-name since implementation might need to
;; capture bindings.
(define-syntax-rule (ai-hold e)      (ai-app ai-hold/n  (ai-lambda () e)))
(define-syntax-rule (ai-setup e0 e1) (ai-app ai-setup/n (ai-lambda () e0) (ai-lambda () e1)))


;; Basic map/fold operation, similar to Racket's list comprehensions.
;; This sequences a siso system, collecting accumulators and output streams/arrays.
(define-syntax-rule
  (ai-loop
   (i (n size))  ;; i = [0..(n-1)]  ;; FIXME: allow (i n) -> infer size elsewhere?
   ((a a0) ...)  ;; Accumulators
   ((v vs) ...)  ;; Vector inputs
   body)
  (ai-app ai-for/n
          (:: Array size)
          (ai-lambda (a ... v ... i n) body)
          (list a0 ...)
          (list vs ...)))


;; (define-syntax const
;;   (syntax-rules ()
;;     ((_ type expr) (ai-app ai-const (:: . type) expr))
;;     ;; ((_ expr)      (ai-app ai-const (ai-current-type) expr))
;;     ))

(define-syntax cast
  (syntax-rules ()
    ((_ type expr) (ai-app ai-cast/n type expr))))

;;; *** Surface syntax for primitives ***


;; Macros below generate the boilerplate for defining the semantics
;; struct and the wrapper functions for all primitives.

(define-syntax (define-prim stx)
  (syntax-case stx ()
    ((_ name arity)
     (let* ((n (syntax->datum #'arity))
            (prim-name ((syntax-prefix "ai-") #'name))
            (prim-impl ((syntax-prefix "ai-impl-") #'name))
            (args (generate-temporaries (make-list n 'a))))
       ;; First parameter passed to all functions is the semantics
       ;; struct.  For each primitive we define a wrapper that calls
       ;; the function in the proper field in this struct.
       #`(define #,prim-name
           (make-ai-function
            (lambda (defs #,@args)
              ((#,prim-impl defs) defs #,@args))
            '#,args))))))

(define (ai-not-defined p)
  (lambda _ (error 'ai-not-defined (format "~s" p))))

(define (ai-prim-default sym op)
  (case sym
    ;; The 'tag primitive is annotation only, and has no
    ;; representation in language semantics.  FIXME: This is probably
    ;; an indication of some broken abstraction.  Currently only used
    ;; for passing hints to the GUI.
    ((tag)   (lambda (sem node record) node))

    ((debug) (lambda (sem v) v))
    
    (else (ai-not-defined sym))))

(define-syntax (declare-prims stx)
  (syntax-case stx ()
    ((_ ai-impl make-ai (p a) ...)
     #`(begin
         ;; Struct with ai-impl-<prim> accessors
         (define-struct ai-impl (p ...))
         ;; Accessors wrapped in struct ai-function with proper arity.
         (define-prim p a) ...
         ;; Keyword interface for language semantics, to avoid
         ;; positional errors.
         (define (make-ai
                  #:default  (default ai-prim-default)  ;; remove
                  #,@(syntax-append*
                      (map (lambda (param)
                             (syntax-case
                                 (list (keyword param)
                                       param
                                       ((syntax-prefix "ai-") param))
                                 ()
                               ((kw p acc)
                                #'(kw (p (default 'p acc))))))
                           (syntax->list #'(p ...)))))
           (make-ai-impl p ...))))))



;; List all primitives with arity.

;; The /n suffix means the arguments are call-by-name.  I.e. there is
;; a driver macro that inserts a (ai-lambda ...) form around the
;; arguments.  All other primitives are call-by-value, following the
;; host Scheme's evaluation order.

;; For metaprogramming, primitives are accessible using the ai- prefix
;; to avoid name clashes.  They take the semantics struct ai-impl as
;; first argument.

(declare-prims ai-impl make-ai


    ;; SPACE/TIME OPERATIONS
    (feedback/n 4)   ;; output feedback / unit time delay
    (for/n 4)        ;; general fold/map

    ;; BLOCK-BASED / SUBSAMPLING OPERATIONS
    (setup/n 2)      
    (hold/n 1)

    ;; MISC LANGUAGE ELEMENTS
    (literal 1)
    (copy 1)         ;; node copy.  (used in type bridging and implementation of hold).
    (index 3)        ;; array indexing

    ;; LIFTED SCALAR OPS
    (add 2)
    (sub 2)
    (mul 2)
    (div 2)
    (pow 2)          ;; second argument = power, should be a number
    (and 2)
    (or  2)
    (xor 2)
    (not 1)
    
    (mod 2)
    (floor 1)        ;; integer s.t. 0 <= x - floor(x) <= 1  (not "< 1" !!!)
    (sin 1)
    (cos 1)
    (exp 1)
    (log 1)
    (atan 1)
    (cast/n 2)       ;; type casting

    (lt 2)           ;; a < b

    ;; CONDITIONAL
    (if 3)
    
    ;; AD-HOC EXTENSIONS
    (dl-shift 2)     ;; shift delay line state vector
    (dl-ref 2)       ;; read from delay line state vector
    
    (tag 2)          ;; tag node with arbitrary list of pairs dictionary '((k . v) ...)
    (debug 1)


    ;; EXPERIMENTAL
    (_map 2)

    

    )

(define-syntax (ai-define stx)
  
  (define (s0 states)
    (for/list ((state (syntax->list states)))
      (syntax-case state ()
        ((s s0) #'(s s0))
        (s      #'(s 0)))))
          
  (syntax-case stx ()

    ;; Full routine with state setup and time coordinates.
    ((_ (name (s ...) (i ...) (t ...) . hack) update)
     #`(define name
         (ai-lambda-feedback
          (#,(s0 #'(s ...)) (i ...) (t ...))
          update)))

    ;; Simple time-invariant feedback form.
    ((_ (name (s ...) (i ...)) form)
     #`(define name
         (ai-lambda-feedback
          (#,(s0 #'(s ...)) (i ...) ())
          form)))

    ;; Pure stream function.
    ((_ (name i ...) form)
     #`(define name (ai-lambda (i ...) form)))

    ;; Define a scheme value.
    ((_ name form)
     #`(define name form))
    
    ))


;;; Convenience

;; Mapper for currying parent ops.
(define (ai-delegate-semantics parent-semantics)
  (lambda (ai-program)
    (lambda args
      (apply (ai-function-proc ai-program)
             parent-semantics args))))
 


;; Data construction / destruction.

(define-syntax ai-vector
  (syntax-rules ()
    ((_ expr ...)
     (list expr ...))))
(define-syntax ai-matrix
  (syntax-rules ()
    ((_ (expr ...) ...)
     (list (list expr ...) ...))))



;; General remarks.

;; * The idea is inspired by the implementation of Haskell type classes.
;;   A more general approach is described in this paper:
;;   http://repository.readscheme.org/ftp/papers/sw2005/garcia.pdf

;; * What about turning ai-lambda into a tree walker which inserts a
;;   lexically bound `app' instead of relying on the module #%app?
;;   This allows embedding processor code in scheme directly (locally)
;;   instead of requiring global bindings.
