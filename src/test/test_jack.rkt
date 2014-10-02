#lang s-exp "stream.rkt"
(require "stream-lib.rkt")
(provide (all-defined-out))

(define main-nsi 0)
(define main-tc '())

  

(define (main samplerate)
  (let*-values
      (((isr)   (/ 1 samplerate))
       ((t)     (* isr (nats)))
       ((f_osc) (* t 1000 isr))
       ((p)     (* .25 (phasor f_osc -.5 .5))))
    (* .125 p)))


  
