#lang racket/base

(require rsound  ;; Install via package manager
         "ai-proc.rkt"
         "libproc.rkt"
         ffi/vector
         "f32vector.rkt")

;; Plug ai-proc / ai-stream into the signal? type.

;; From looking at the code:
;;
;;  - A signal is a network witout inputs.
;;
;;  - A network is a function or a (in out maker) triplet.  The maker
;;    returns a stateful procedure.
;;
;;  - Networks have one output.
;;

(define test-signal
  (let ((state 0))
    (network/s 0 1
               (lambda ()
                 (lambda ()
                   (set! state (+ state .01))
                   (sin state))))))

(define (ai-network f)
  (let* ((class (ai-proc f #:nsi #f))  ;; no param inputs
         (nin  (proc-class-nin  class))
         (nout (proc-class-nout class)))
    (network/s
     nin nout
     (lambda ()
       (let ((inst (proc-instantiate class))
             ;; Run at blocksize = 1
             (vin  (for/list ((i (in-range nin)))  (make/init-f32vector 1)))
             (vout (for/list ((i (in-range nout))) (make/init-f32vector 1))))
         (lambda args
           (for ((a args) (v vin)) (f32vector-set! v (+ 0.0 a)))
           (proc-run! inst
                      (if (zero? nin) 1 vin)
                      vout) 
           (apply values
                  (for/list ((v vout))
                    (f32vector-ref v 0)))))))))


;; Define some rai generators using the DSL.
(begin
  (module test-progs "stream.rkt"
    (require "synth-lib.rkt") ;; saw-d1
    (provide (all-defined-out))
    (define (test-saw) (saw-d1 .001)))
  (require 'test-progs))


(define (test [s test-signal])
  (signal-play s)
  (sleep 1)
  (stop))

(test)



