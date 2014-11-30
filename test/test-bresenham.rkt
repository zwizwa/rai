#lang racket/base
(require rai/ai-stream
         rai/tools
;         rai/stream-syntax
;         rai/stream-lib
;         "test_pd.rkt"
;         "test-lang.rkt"
         )

;; Simple test macro.
(define-syntax-rule (t . a)
  (begin
    (display ";; ")
    (pretty-print '(t . a))
    (ai-stream . a)))

;; Some stream functions
(begin
  (module dsp rai/stream
    (provide (all-defined-out))
    (define (test (s) ())
      (values (and (+ s 1) #xF) s))
    )
  (require 'dsp))


;; Transpose
(define (@ x)
  (apply values
         (transpose
          (sequence-take
           (sequence-map list x)
           20))))
;; Multi-valued
(define-syntax-rule (@s . e)
  (apply values (map @ (values-list . e))))

(define res
  (@ ((t test) (make-list 20 0))))
res
