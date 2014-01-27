#lang racket

(require "ai-proc.rkt"
         "stream-lib.rkt" ;; integrate
         ffi/vector
         "f32vector.rkt")

(begin
  (module test-progs "stream.rkt"
    (provide (all-defined-out))
    (define (sum a b) (+ a b)))
  (require 'test-progs))


(define vins  (list (make/init-f32vector 10 1.0)))
(define vouts (ai-proc-run-once (ai-proc integrate) vins))

(map f32vector->list vouts)

(map f32vector->list (ai-proc-run-once (ai-proc sum)
                                       (append vouts vouts)))


