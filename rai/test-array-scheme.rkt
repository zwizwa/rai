#lang racket/base
(require
 "tools.rkt"
 "ai-array-scheme.rkt"

 ;; Transformer environment references to abstract programs passed to
 ;; ai-array-scheme.
 (for-syntax
  racket/base
  "stream-lib.rkt"
  "test-lang.rkt"))

(define (test)
  (define-values
    (loop si so i p o)
    (ai-array-scheme nphasor))
  loop)

    