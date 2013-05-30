#lang setup/infotab
(define name "RAI")
(define blurb
  '("Analysis and Code Generation for DSP algorithms through Abstract Interpretation."))
(define repositories '("4.x"))
(define primary-file '("stream.rkt" "ai-array-c.rkt" "ai-freq.rkt"))
(define homepage "http://zwizwa.be/rai")
(define categories '(devtools metaprogramming))
(define scribblings '(("doc/rai.scrbl" ())))
(define release-notes
  '(div
    "See the "
    (a ((href "http://zwizwa.be/rai")) "RAI website")
    " for release notes."))
(define version "1.0")
