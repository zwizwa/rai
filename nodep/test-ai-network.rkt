#lang racket/base

(require rsound  ;; Install via package manager
         rai/meta
         rai/ai-network)

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
  (module test-generators rai/stream
    (require rai/synth-lib) ;; saw-d1
    (provide (all-defined-out))
    (define sawtooth0 saw-d1)
    (define (sawtooth1) (saw-d1 .001))
    (define (sawtooth2) (saw-d2 .002))
    (define (sawtooth3) (saw-d3 .003)))
  (require 'test-generators))


(define (test-play s)
  (signal-play s)
  (sleep 1)
  (stop))

(define sawtooth0_ (ai-network sawtooth0))
;(define vibrato-tone
;  (network ()
;           [lfo (sine-wave 5)]
;           [sig (sawtooth0_ (* .01 lfo))]
;           [out (* 0.5 sig)]))

(define vibrato-tone
  (network ()
           [lfo = (sine-wave 5)]
           [sig = (sawtooth0_ (* .01 lfo))]
           [out = (* 0.5 sig)]))

(define (test)
  (test-play reference)
  (for ((s (list sawtooth1
                 sawtooth2
                 sawtooth3)))
    (test-play (ai-network s)))
  (test-play vibrato-tone)
  )


;; FIXME: rsound api changed..
;; (test)



