#lang racket/base
(require rai/ai-array-c
         rai/stream-syntax
         rai/stream-lib
         "test-lang.rkt")

;;(display (ai-c integrate3))
;;(display (ai-c twopole))
;;(display (ai-c level-test))

(module dsp rai/stream
  (require rai/stream-lib)
  (provide (all-defined-out))
  (define (test-edge x)
    (cast Float (positive-edge x))))
(require 'dsp)

(display (ai-array-c phasor)) ;; #:name 'phasor))

;; (display (ai-c test-peval))

(display (ai-array-c test-edge))
