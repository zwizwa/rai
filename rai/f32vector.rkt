#lang racket/base
(require ffi/vector
         ffi/unsafe)

(provide (all-defined-out))

(define (make/init-f32vector n [val 0.0])
  (let ((vec (make-f32vector n)))
    (for ((i (in-range n))) (f32vector-set! vec i val))
    vec))

(define (ptr-set-f32vectors! arr vs)
  (when arr
    (for ((i (in-naturals)) (v vs))
      (ptr-set! arr _pointer i (f32vector->cpointer v)))))

