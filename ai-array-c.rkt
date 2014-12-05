#lang racket/base
(require "tools.rkt"
         "type.rkt"
         "ai-array.rkt")
(provide (all-defined-out))

(define (c-initializer dims val)
  (cond
   ((null? dims) val)
   ((zero? val) "{}") ;; prevents all-zero initializers to get too large
   (else
    (c-initializer
     (cdr dims)
     (format "{~a}"
             (c-list 
              (make-list (car dims) val)))))))



;; Generate C types and code from generic imperative block processing
;; program.

;; TODO: It might be interesting to generate a function with implicit
;; state for single-shot or inifinite processes (e.g. threads).  This
;; allows defining all state variables as local variables in the loop,
;; giving more opportunity for optimization to the compiler.


(define (ai-array/c expr
                    #:indextype (indextype 'int)
                    #:prefix (prefix "proc_")
                    #:defaults (defaults '())
                    #:default_type (default_type "float32_t"))
  
  (define (pfx x) (format "~a~a" prefix x))

  ;; FIXME: For unused variables, type is unknown.  Might just as well
  ;; not include them, i.e. expand as "//" to not define them, but
  ;; this requires the parameter macros to be adapted to not iterate
  ;; over undefined names.
  
  (define ctype
    (match-lambda
     ((struct type ('Float     #f '())) "float32_t")
     ((struct type ('Int       #f '())) "int32_t")
     ((struct type ('Undefined #f '())) default_type)
     ((struct type-var (_)) default_type) ;; FIXME: should not happen
     (type (error 'ctype (format "~a" type)))))
  
  (define *external-nodes* (make-hash))
  (define (node-name node) (caddr node))
  (define (node-dims node) (cadr node))
  (define (node-base-type node) (car node))
  (define (node-kind node) (length (node-dims node)))
  (define (register-nodes! ts/nss)
    (for ((t/ns ts/nss))
      (apply (lambda (t ns)
               (for ((n ns)) (hash-set! *external-nodes* (node-name n) t))
               ) t/ns)))

  ;; Connect node name to C function input references.
  (define (bind-node name)
    (let ((namespace (hash-ref *external-nodes* name false)))
      (if namespace
          ;; External node
          (format "~a->~a" namespace name)
          ;; Internal node
          (format "~a" name))))
  
  (define nest (make-parameter 0))
  (define (with-nest thunk)
    (parameterize ((nest (add1 (nest)))) (thunk)))
  (define tab-string
    "    "
    ;; "\t"
    )
  (define (tab1) (display tab-string))
  (define (tab) (for ((i (in-range (nest)))) (tab1)))

  (define (arrayref lst)
    (define (sqbr x) (format "[~a]" x))
    (apply string-append (map sqbr lst)))

  ;; Generate the structured state and param nodes.
  ;; FIXME: space in " \n" is a hack to make scribble output work
  (define (gen-type tag nodes)
    (define stream
      (case tag
        ((in out) "* restrict ")
        (else "")))
    (printf "struct ~a {\n" (pfx tag))
    (for ((n nodes)
          (i (in-naturals)))
      (match n
        ((list base-type dims name)
         (tab1) (printf "~a ~a~a~a;\n"
                        (ctype base-type)
                        stream ;; t-indexed uses pointers
                        name
                        (arrayref (reverse dims))))))
    (printf "};\n"))


  (define (gen-preproc tag nodes)
    
    ;; Some C preprocessor annotation for the i/o nodes.

    ;; Per node define, useful for testing presence of a certain
    ;; parameter at compile time.
    (for ((n nodes))
      (printf "#define ~a_~a ~a\n"
              (pfx tag)
              (node-name n)
              (node-kind n)))

    ;; Iteration over all nodes, useful for building indexing
    ;; structures and/or code.
    ;; FIXME: the parameter name can conflict with a variable name.
    (printf "#define ~a_~a(__macro__) \\\n" (pfx "for") tag)
    (for ((n nodes))
      ;; (pp n)
      (printf "~a__macro__(~a) \\\n"
              tab-string
              (c-list `(,(node-name n)
                        ,(ctype (node-base-type n))
                        ,(node-kind n)
                        ,(foldr * 1 (node-dims n))
                        ,@(reverse (node-dims n))))))
    (printf " \n"))

  (define (gen-param-defaults)
    (printf "#define ~a(__macro__) \\\n" (pfx "for_param_defaults"))
    (for ((d (in-dict-pairs defaults)))
      (printf "~a__macro__(~a) \\\n"
              tab-string
              (c-list (list (car d) (cdr d)))))
    (printf " \n"))
      

  (define (ref expr)
    (match expr
      ((list-rest '@ var index)
       (format "~a~a"
               (bind-node var)
               (arrayref index)))
      (else
       (if (number? expr)
           (exact->inexact expr)
           expr))))
  
  (define (format-expr expr)
    ;; (pp expr)
    (match expr
      ((list-rest op args)
       (format "~a(~a)" op (c-list (map ref args))))))
  (define (line . a)
    (tab) (apply printf a))

  (define (state-pointer type/name index)
    (line "struct proc_~a * restrict ~a = (struct proc_~a *)(&state[(~a)&1]);\n"
          type/name type/name type/name
          index))


  (define (gen-statement expr)
    ;; (pp expr)
    (match expr
      ((list-rest 'block statements)
       (for ((s statements))
         (gen-statement s)))

      ((list 'p_loop (list i s e) expr)
       (begin
         (line "for (~a ~a = ~a; ~a < ~a; ~a ++) {\n" indextype i s i e i)
         (with-nest
          (lambda ()
            (when (eq? 't i)
              ;; Temporal loop swaps state buffers.
              (state-pointer "si" "t^0")
              (state-pointer "so" "t^1"))
            (gen-statement expr)
            ))
         (line "}\n")))

      ;; Local variable assignment (accumulator update)
;      ((list '()
;             (list-rest 'p_set dst src index))
;       (line "~a = ~a;\n" dst (ref src)))

      ;; Array assignment.
      ((list (list-rest '! name index) expr)
       (let ((indx (if (null? index) ""
                       (arrayref index))))
         (line "~a~a = ~a;\n"
               (bind-node name)
               indx (format-expr expr))))
      
      ;; Temp array declaration.
      ((list (list type name dims) _)
       (line "~a ~a~a;\n"
             (ctype type) name (arrayref dims)))

      ;; Local variable definition.
      ((list (list type name) expr)
       (line "~a ~a = ~a;\n"
             (ctype type) name
             (format-expr expr)))
      ))

  (define (gen-node-units units)
    ;; See prims.h for param/field order
    (define fields '(name unit min max scale curve))
    (define ((fld d) f)
      (let ((v (dict-ref d f)))
        (cond
         ((string? v) (format "~s" v))
         (else v))))
    (line "#define ~a(m) \\\n" (pfx "for_control"))
    (for (((node node-info) (in-dict units)))
      (with-nest
       (lambda ()
         (line "m(~a,~a) \\\n"
               node (c-list (map (fld node-info) fields))))))
    (line " \n"))

  

  (define (gen-node-inits inits si)
    (for ((n si))
      (match n
        ((list type dims name)
         (let ((v (dict-ref inits name 0)))
           ;; Note this doesn't use the 'si' tag\.  Reason: reference
           ;; in main_sp.c is simpler by name only.  OK since name is
           ;; unique.
           (line "#define ~a_init ~a\n"
                   (pfx name)
                   (c-initializer dims v))))))
    (line " \n"))
    
  (define (gen-code)
    (match expr
      ((list _
             meta
             (list si so in param out store)
             expr)
       ;; The state struct is represented by 2 isomorphic types: si
       ;; so, with different node names to allow ping-pong buffer
       ;; implementation.
       (let ((units (dict-ref meta 'units))
             (inits (dict-ref meta 'inits))
             (nodes (map list
                         '(si so in param out store)
                         (list si so in param out store))))
         (register-nodes! nodes)

         ;; C structs for si so in param out store
         (for ((t/n nodes)) (apply gen-type t/n))

         ;; Preprocessor meta info, useful for building structures and
         ;; code at compile time.  
         (for ((t/n nodes)) (apply gen-preproc t/n))

         ;; GUI parameters
         (gen-node-units units)

         ;; Node initializers.  Note that params don't have
         ;; initializers = a pure input.  However, implementation is
         ;; stateful, so they will be to 0 at in proc_instance_new().
         (gen-node-inits inits si)
         (gen-node-inits '() store)  ;; FIXME: currently all 0

         ;; Param defaults
         (gen-param-defaults)

         ;; Core routine
         (line "static void ~a (\n" (pfx "loop"))
         (with-nest
          (lambda ()
            (let ((arg
                   (lambda (type name)
                     (line "struct ~a * restrict ~a,\n"
                           (pfx type) name))))
              (arg "si"    "state")
              (arg "in"    "in")
              (arg "param" "param")
              (arg "out"   "out")
              (arg "store" "store"))
            (line "~a t_endx) {\n\n" indextype)))
         (with-nest
          (lambda ()
            ;; Unrolled part needs si/so pointers defined.
            (state-pointer "si" "0")
            (state-pointer "so" "1")
            (gen-statement expr)))
         (printf "}\n")
         ))))

  ;; (stderr (gen-code))
  (with-output-to-string gen-code))

;(define-syntax-rule (@ . expr)
;  (let ((vs (values-list expr)))
;    (pretty-print `(expr ,vs))
;    (apply values vs)))

;; Compiler driver.
(define (ai-array-c program
                    #:defaults (defaults '())
                    #:tc (ignored #f)
                    #:nsi (nsi #f))   ;; Default: all inputs are streams
  (ai-array/c
   (ai-array program
             #:out-base Float
             #:nsi nsi)
   #:defaults defaults))

