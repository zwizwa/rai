#lang racket/base
(require racket/match
         "tools.rkt"
         "stream-syntax.rkt"
         "stream-meta.rkt"
         )

(provide ai-dual   ;; Raw operation on dual numbers
         ai-deriv  ;; (partial) derivative
         (struct-out dual))
          

;; Automatic differentiation transforms an abstract program that
;; computes the value of a function into an abstract program that
;; computes the value of the first derivative.

;; Implemented using abstract interpretation over dual numbers.
;; x + dx e, with e^2 = 0
;; http://en.wikipedia.org/wiki/Dual_numbers
;; http://en.wikipedia.org/wiki/Automatic_differentiation



  

;; Lift program over dual number domain.
(define (ai-dual program)
  (define (dual-proc parent-semantics . args)

    ;; Define an operation on dual numbers using pattern matching.



    ;; FIXME: special-case the binary ops in case the operands are
    ;; equal -- e.g. (mul x x) -- to avoid loosing
    ;; symmetry/sharing/partial-eval opportunities.
    
    ;; Shorthands for parent primitives.
    (define @ (ai-delegate-semantics parent-semantics))
    
    (define +   (@ ai-sum))
    (define -   (@ ai-sub))
    (define *   (@ ai-prod))
    (define /   (@ ai-div))
    (define exp (@ ai-exp))
    (define sin (@ ai-sin))
    (define cos (@ ai-cos))
    (define log (@ ai-log))
    (define pow (@ ai-pow))
    (define lt  (@ ai-lt))
    (define lit (@ ai-literal))

    (define (lift-constant x)
      (if (dual? x) x (make-dual x (lit 0))))
    (define-syntax op-match
      (syntax-rules ()
        ((_ ((v d) ...) expr)
         (lambda args
           (match (map lift-constant args)
             ((list sem-ignored (struct dual (v d)) ...)
              expr))))))
    (define-syntax op
      (syntax-rules ()
        ((_ bindings e-v e-dv)
         (op-match bindings (make-dual e-v e-dv)))))
    

    ;; Note that `pow' is redundant as it can be written in terms of
    ;; `log' and `exp', but the primitive is there to allow exact math
    ;; when de = 0 and b is an integer.
    (define d-pow
      (op-match
       ((b db) (e de))
       (let ((b^e (pow b e))) ;; maintain sharing
         (make-dual b^e
                    (+ (* db e (pow b (- e 1)))
                       (* de (log b) b^e de))))))

    (define d-lt
      (op
       ((a da) (b db) (x dx) (y dy))
       (lt a b x y)
       (lt a b dx dy)))

    (define ai-dual-semantics
      ;;             Given value, deriv   compute value, deriv
      (make-ai #:add (op ((a da) (b db))  (+ a b)   (+ da db))
               #:sub (op ((a da) (b db))  (- a b)   (- da db))
               #:mul (op ((a da) (b db))  (* a b)   (+ (* da b) (* a db)))
               #:div (op ((a da) (b db))  (/ a b)   (- (/ da b) (/ (* a db) (* b b))))
               #:exp (op ((x dx))         (exp x)   (* (exp x) dx))
               #:sin (op ((x dx))         (sin x)   (* (cos x) dx))
               #:cos (op ((x dx))         (cos x)   (* -1 (sin x) dx))
               #:log (op ((x dx))         (log x)   (pow x -1))
               #:pow d-pow
               #:lt  d-lt

               #:literal (op ((x dx)) x dx)
               ))
        
    (apply (ai-function-proc program)
           ai-dual-semantics
           args))
  
  ;; Return transformed program.
  (make-ai-function
   dual-proc
   (ai-function-args program)))
  
(define (ai-deriv program [index 0])
  
  (define (derivative-proc parent-semantics . args)
    (define (literal value)
      ((ai-function-proc ai-literal)
       parent-semantics
       value))
    ;; Derivative is computed wrt. function argument at index, so that
    ;; argument has derivative one.  All arguments are constant
    ;; parameters for the partial derivative, so have derivative 0.
    (define (arg-deriv i)
      (literal (if (= i index) 1 0)))
    (define dual-args
      (for/list ((a args)
                 (i (in-naturals)))
        (make-dual a (arg-deriv i))))
    (define results (values-list
                     (apply (ai-function-proc (ai-dual program))
                            parent-semantics
                            dual-args)))
    (apply values (map dual-dx results)))
  
  (make-ai-function
   derivative-proc
   (ai-function-args program)))

  
