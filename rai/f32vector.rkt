#lang racket/base
(require ffi/vector)

(provide (all-defined-out))

(define (make/init-f32vector n [val 0.0])
  (let ((vec (make-f32vector n)))
    (for ((i (in-range n))) (f32vector-set! vec i val))
    vec))
