#lang racket/base
(require rai/ai-array-c)

(provide (all-defined-out))

(define (reload)
  (define ns (make-base-namespace))
  (eval `(begin
           (require rai/ai-array-c)
           (require (file "synth.rkt"))
           (display (ai-array-c main #:nsi main-nsi)))
        ns))

  
