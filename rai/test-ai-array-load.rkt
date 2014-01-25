#lang racket

(require "ai-proc.rkt"
         "stream-lib.rkt" ;; integrate
         ffi/vector)

  
(define sp-class (ai-proc integrate))

(define ins  (list (list->f32vector (map exact->inexact '(1 1 1 1 1 1)))))
(define outs (map f32vector->list (ai-proc-run-once sp-class ins)))

outs


