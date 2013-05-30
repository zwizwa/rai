#lang racket/base
(require "tools.rkt"
         "stream-syntax.rkt")
(provide peval)

;; Shared routines for partial evaluation based on prim.rkt and some
;; algebraic rules like add/mul of 0 and or 1.

;; Perform partial evaluation of numeric operations if all arguments
;; are known at compile time.

;; We could leave most of this to the C compiler.  However, it makes
;; sense in case of multiplication by 0 since it can eliminate whole
;; subtrees, significantly reducing complexity of the generated code
;; and reducing C compile time.  This allows guilt-free building of
;; very generic building blocks that get pruned when some
;; coefficients are 0.

;; FIXME: Take into account commutative laws.  This approach doesn't
;; optimize (+ 1 (+ 2 y)) into (+ 3 y)




(define (peval sem   ;; semantics for partial evaluation
               fn    ;; function primitive
               op    ;; operator syntax for compilation
               args) ;; input nodes or literals
  (define (@ op . args)
    (apply (ai-function-proc op) sem args))

  (define (unpack-number x)
    (let ((d (->datum x)))
      (if (number? d) d x)))
  (define args-n
    (map unpack-number args))
  (define form
    (cons (->datum op) args-n))

  (define result
    (if (andmap number? args-n)
        ;; All numeric -> full eval is possible.
        (apply fn args-n)
        ;; Some operations can be simplified if one of the arguments
        ;; is 0/1 while the other one remains symbolic.
        (match form
          ((list 'p_add x 0) x) ((list 'p_add 0 x) x)
          ((list 'p_sub x 0) x)
          ((list 'p_mul x 1) x) ((list 'p_mul 1 x) x)
          ((list 'p_mul x 0) 0) ((list 'p_mul 0 x) 0)
          ((list 'p_div 0 x) 0)
          ((list 'p_div x 1) x)
          ((list 'p_div x (? number? n)) `(p_mul ,x ,(@ ai-div 1 n)))
          ((list 'p_pow x 1) x)
          ((list 'p_pow x 0) 1)
          (else
           ;; Can't do anything so create an operation node.
           (cons op args)
           ))))
  
  ;; (stderr-pp `(,op ,args-n, result))
  result)

