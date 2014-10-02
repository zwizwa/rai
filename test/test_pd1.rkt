#lang s-exp "stream.rkt"
(require "stream-lib.rkt")
(provide (all-defined-out))

(define main-nsi 1)
(define main
  (lambda (inc min max)
    (phasor (* inc 0.05) min max)))

