#lang racket/base
(require rai/tools
         rai/ai-stream)
(provide (all-defined-out))

;; Originally from test-ai-stream.rkt
;; Some simple macros for testing sequences on the command line or in a script.

;; Simple test macro.
(define-syntax-rule (t . a)
  (begin
    (display ";; ")
    (pretty-print '(t . a))
    (ai-stream . a)))

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
