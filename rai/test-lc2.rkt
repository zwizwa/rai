#lang racket
(require portaudio
         "ai-proc.rkt"
         "libproc.rkt"
         ffi/vector)

;; Second generation livecoding environment.  Based on
;; state-preserving code switches.  This is a bit tricky.

;; FIXME: For some reason this really doesn't want to run properly..
;; It's probably best to use jack directly, since the code is C and
;; can run in real time.

(module dsp "stream.rkt"
  (require "synth-lib.rkt")
  (provide (all-defined-out))

  (define (main-generator)
    (values (clip1 (saw-d1 .001))
            (clip1 (saw-d1 .00101))))
  )

(require 'dsp)


(define sample-rate 44100.0)
(define (real->s16 x)
  (inexact->exact (round (* 32767 x))))
(define class (ai-proc main-generator))

(unless (and (= 0 (proc-class-nin class))
             (= 2 (proc-class-nout class)))
  (error 'io-arity))

(define instance (proc-instantiate class))

(define bufsize 0)
(define L/R #f)
(define (check-buffers! size)
  (when (< bufsize size)
    (let ((new-bufsize size))
      (printf "new-bufsize ~a\n" new-bufsize)
      (set! L/R
            (list
             (make-f32vector new-bufsize)
             (make-f32vector new-bufsize)))
      (set! bufsize new-bufsize))))

(check-buffers! 10000)

;; Block size is variable.  Why?
(define (buffer-filler setter nb-frames)
  ;;(printf "~a\n" nb-frames)
  (when (< bufsize nb-frames)
    (set! nb-frames bufsize))
  (for ([i (in-range nb-frames)])
    (proc-run! instance nb-frames L/R)
    (for ((frame '(0 1)))
      (setter (+ frame (* i 2))
              (real->s16
               (f32vector-ref (list-ref L/R frame) i))))))

(define stop void)

(define (start)
  (match-define (list timer stats stopper)
                (stream-play buffer-filler .2 sample-rate))
  (set! stop stopper))

(start)
(sleep 3)
(stop)
