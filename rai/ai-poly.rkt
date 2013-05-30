#lang racket/base

(require "stream-syntax.rkt"
         "peval.rkt"
         "prim.rkt"
         "tools.rkt"
         "ai-eval.rkt"
         )
(provide ai-poly (struct-out poly))

(define-struct poly (coefs) #:transparent)



(define (ai-poly program)
  (define (proc parent-semantics . args)
    (define (parent prim)
      (lambda args
        (apply (ai-function-proc prim)
               parent-semantics
               args)))
    (define (zeros n)
      (if (> n 0)
          (make-list n 0)
          '()))
    (define (poly-lift prim)
      (lambda (sem pa pb)
        (let*
            ((as (poly-coefs pa)) (na (length as))
             (bs (poly-coefs pb)) (nb (length bs))
             (as (append as (zeros (- nb na))))
             (bs (append bs (zeros (- na nb)))))
          (make-poly
           (map (parent prim) as bs)))))
    (define poly-add (poly-lift ai-add))
    (define poly-sub (poly-lift ai-sub))

    (define mul (parent ai-mul))
    (define add (parent ai-add))
    
    (define (poly-scale p s)
      (make-poly (map (lambda (c) (mul s c)) (poly-coefs p))))
    (define (poly-shift p [n 1])
      (make-poly (append (zeros n) (poly-coefs p))))
    (define (poly-mul sem pa pb)
      (car
       (values-list
        (for/fold
            ((accu (make-poly '()))
             (shift pa))
            ((b (poly-coefs pb)))
          (values (poly-add sem accu (poly-scale shift b))
                  (poly-shift shift))))))
    (define ai-poly-semantics
      (make-ai #:add poly-add
               #:sub poly-sub
               #:mul poly-mul
               #:literal (lambda (_ x) x)))
    (apply (ai-function-proc program)
           ai-poly-semantics
           args))
  (make-ai-function
   proc
   (ai-function-args program)))


(define (test-prim prim)
  ((ai-function-proc (ai-poly prim))
   ai-eval-semantics
   (make-poly '(1 1 1))
   (make-poly '(1 1))))
(define (test)
  (list
   (test-prim ai-add)
   (test-prim ai-sub)
   (test-prim ai-mul)))
;;(test)
