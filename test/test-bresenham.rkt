#lang racket/base
(require rai/ai-stream
         rai/tools
         rai/test-tools
         rai/stream-lib
;;         rai/stream-syntax
;;         "test_pd.rkt"
;;         "test-lang.rkt"
         )


;; Some stream functions
(module dsp rai/stream
  (provide (all-defined-out))
  (define (bres (s c) (i m p))
    (let* ((next_c (+ c 1))
           (c_p (quot next_c p))
           (next_s (+ s (* c_p i))))
      (values (mod  next_s m)
              (mod  next_c p)
              (quot next_s m)
              ))))
(require 'dsp)

(define (l x)
  (if (list? x)
      (take (apply append (make-list 20 x)) 20)
      (make-list 20 x)))

; (t bres)

(@ ((t bres) (l 3) (l 8) (l 2)))
(@s ((t timer) (l 2)))
(@s ((t gated-timer) (l 2) (l 1)))
