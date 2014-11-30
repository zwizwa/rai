#lang racket/base
(require rai/ai-stream
         rai/tools
         rai/test-tools
;         rai/stream-syntax
;         rai/stream-lib
;         "test_pd.rkt"
;         "test-lang.rkt"
         )


;; Some stream functions
(module dsp rai/stream
  (provide (all-defined-out))
  (define (bres (s) (i m))
    (let ((next (+ s i)))
      (values (mod  next m)
              (quot next m)
              ))))
(require 'dsp)

(define (l x) (make-list 20 x))

; (t bres)

(@ ((t bres) (l 3) (l 8)))
