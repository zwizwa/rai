#lang racket/base

(require rsound  ;; Install via package manager
         "ai-network.rkt")


;; A raw rsound signal? as a reference point.
(define reference
  (let ((state 0))
    (network/s 0 1
               (lambda ()
                 (lambda ()
                   (set! state (+ state .01))
                   (sin state))))))

;; Some signal generators using the stream DSL.
(begin
  (module test-generators "stream.rkt"
    (require "synth-lib.rkt") ;; saw-d1
    (provide (all-defined-out))
    (define (sawtooth1) (saw-d1 .001))
    (define (sawtooth2) (saw-d2 .002))
    (define (sawtooth3) (saw-d3 .003)))
  (require 'test-generators))


(define (test-play s)
  (signal-play s)
  (sleep 1)
  (stop))

(define (test)
  (test-play reference)
  (for ((s (list sawtooth1
                 sawtooth2
                 sawtooth3)))
    (test-play (ai-network s))))

(test)




