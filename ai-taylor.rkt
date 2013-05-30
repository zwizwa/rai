#lang racket/base

(require "stream-syntax.rkt"
         "stream-meta.rkt")
(provide ai-taylor
         ai-tailor-const
         ai-tailor-symbolic
         ;; ai-taylor-take
         )

;; FIXME: A nice idea, but probably not useful due to convergence
;; issues in the function composition operation.  It might be better
;; to use autodiff.


;; A series is represented by a function a(n) over the natural
;; numbers, which represents the function f(x) = \sum_n a[n] x^n

;; FIXME: use memoized functions.

;; Main difficulties are multiplication (convolution) and function
;; composition.

;; Function composition of series is not a finite operation, so some
;; kind of approximation criterion needs to be used.  ( Note: the
;; point is to obtain a sequence representation of a function, but the
;; coefficients themselves will necessarily be approximations. )

(define (ai-tailor-const value [lit (lambda (x) x)])
  (lambda (n) (lit (if (zero? n) value 0))))

(define (ai-tailor-symbolic sym [lit (lambda (x) x)])
  (lambda (n) (lit (string->symbol (format "~a~a" sym n)))))

(define (ai-taylor program)
  (define (series-proc parent-semantics . args)
    (define @   (ai-delegate-semantics parent-semantics))
    
    (define sum (@ ai-sum))
    (define sub (@ ai-sub))
    (define mul (@ ai-mul))
    (define lit (@ ai-literal))

    (define (s-mul _ a b)
      (lambda (n)
        (apply sum
               (for/list ((k (in-range (+ n 1))))
                 (mul (a k) (b (- n k)))))))

    (define (s-compose _ a b)
      #f)

    (define ai-taylor-semantics
      (make-ai #:default
               (lambda (sym fn)
                 (lambda (sem . args)
                   (cons sym args)))
               #:literal (lambda (_ value) (ai-tailor-const value lit))
               #:add (lambda (_ a b) (lambda (n) (sum (a n) (b n))))
               #:sub (lambda (_ a b) (lambda (n) (sub (a n) (b n))))
               #:mul s-mul))
    (apply (ai-function-proc program)
           ai-taylor-semantics
           args))
  (make-ai-function
   series-proc
   (ai-function-args program)))


    
