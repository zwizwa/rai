#lang racket/base
(require "test-lang.rkt"
         "ai-array-c.rkt"
         "stream-lib.rkt"
         "ai-array.rkt")
;; (ai-array/1 z)
(define (test program #:tc [tc '()] #:nsi [nsi 0])
  (display
   (ai-array-c
    program
    ;; twopole
    ;; integrate
    ;; nphasor
    ;; phasor
    #:tc tc
    #:nsi nsi
    )))

(test test-t_endx)
;; (test integrate)
;; (test z)
