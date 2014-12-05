#lang s-exp rai/stream
(require rai/stream-lib)
(provide (all-defined-out))

(define main-nsi 0)
(define main-tc '())
(define main-defaults '())

  

(define (main samplerate)
  (let*-values
      (((isr)   (/ 1 samplerate))
       ((t)     (* isr (nats)))
       ((f_osc) (* t 1000 isr))
       ((p)     (* .25 (phasor f_osc -.5 .5))))
    (* .125 p)))


  
