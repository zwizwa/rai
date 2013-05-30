#lang racket/base
(require "tools.rkt"
         "ai-autodiff.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         "ai-symbolic.rkt"
         "stream-syntax.rkt"
         "stream-lib.rkt"
         "stream-meta.rkt"
         "test-lang.rkt")

(define (test program #:tc [tc '()] #:nsi [nsi 0])
  (display
   (ai-array-c
    program
    ;; twopole
    ;; integrate
    ;; nphasor
    ;; phasor
    #:tc tc
    #:nsi nsi
    )))

;;(test ntwopole #:tc '((N . 3) (M . 4)) #:nsi 1)

;;(test test-poly)


;;(test t-exp)

  
;; Test ai-dual and ai-deriv

(define f-symbolic-0 ((ai-symbolic test-square) 'x))

(define f-dual       (ai-dual test-square))
(define f-symbolic-n ((ai-symbolic f-dual) (make-dual 'x0 1)))

(define f-deriv      (ai-deriv  test-square))
(define f-symbolic-1 ((ai-symbolic f-deriv) 'x))

f-symbolic-0
f-symbolic-1
f-symbolic-n


((ai-symbolic (ai-deriv test-autodiff-1)) 'x)

((ai-symbolic (ai-deriv test-pow-10)) 'x)
((ai-symbolic (ai-deriv test-sqrt)) 'x)
