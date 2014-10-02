#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))

(define main-nsi 0)
(define main
  (lambda ()
    (supersaw .005 .02)))
  
  

