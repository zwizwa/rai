#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))

(define-values
  (main main-defaults)
  (lambda/params (input             ;; signal inputs
                  samplerate freq)  ;; parameter inputs
    (let* ((freq (/ freq samplerate))
           (saw  (* .1 (saw-d3 freq))))
      (+ input saw)                 ;; one (unnamed) output
      )))

;; Number of signal inputs.  The rest are parameters.
(define main-nsi 1)
(define main-tc '())
