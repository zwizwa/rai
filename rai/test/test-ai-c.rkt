#lang racket/base
(require "ai-array-c.rkt"
         "stream-syntax.rkt"
         "stream-lib.rkt"
         "test-lang.rkt")

;;(display (ai-c integrate3))
;;(display (ai-c twopole))
;;(display (ai-c level-test))


(display (ai-array-c phasor #:name 'phasor))

;; (display (ai-c test-peval))

