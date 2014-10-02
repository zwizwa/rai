#lang racket

(require rai/ai-proc
         rai/libproc
         rai/stream-lib ;; integrate
         ffi/vector
         rai/f32vector)

(begin
  (module test-progs rai/stream
    (provide (all-defined-out))
    (define (sum a b) (+ a b)))
  (require 'test-progs))


(define vins  (list (make/init-f32vector 10 1.0)))
(define vouts (proc-run-once (ai-proc integrate) vins))

(map f32vector->list vouts)

(map f32vector->list (proc-run-once (ai-proc sum)
                                    (append vouts vouts)))


