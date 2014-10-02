#lang racket/base
(require "ai-array-c.rkt")

(provide (all-defined-out))

(define (reload)
  (define ns (make-base-namespace))
  (eval `(begin
           (require "ai-array-c.rkt")
           (require (file "synth.rkt"))
           (display (ai-array-c main #:nsi main-nsi)))
        ns))

  
