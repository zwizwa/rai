#lang s-exp rai/stream
(require rai/stream-lib)
(provide (all-defined-out))

(define main-nsi 1)
(define main
  (lambda (inc min max)
    (phasor (* inc 0.05) min max)))

