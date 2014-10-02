#lang s-exp "stream.rkt"
(require "stream-lib.rkt"
         "synth-lib.rkt")
(provide (all-defined-out))

(define main-nsi 0)
(define main
  (lambda ()
    (supersaw .005 .02)))
  
  

