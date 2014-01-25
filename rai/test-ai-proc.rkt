#lang racket

(require "ai-proc.rkt"
         "stream-lib.rkt" ;; integrate
         ffi/vector
         "f32vector.rkt")


(define proc-class (ai-proc integrate))

(define ins  (list (make/init-f32vector 10 1.0)))
(define outs (map f32vector->list (ai-proc-run-once proc-class ins)))

outs


