#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))

;; Control space exploration

;; Sound control parameters are not just numbers.  They are numbers
;; within a range.  The minimum and maximum assocated to this range
;; are _essential_ to what that number represents.

;; To get good parameter control, it is necessary to find the maximum
;; and minimum.  One way to do this is to:
;; - Given its range, set the parameter using a fixed-range knob
;; - Set the min/max using the "quoted parameter" approach in RAI

;; The latter does not have any limits apart from the floating point
;; range: it can essentially go infinitely high and low in 10%
;; increments/decrements.

;; This is a doodle-style file to explore the principle.


(define main-nsi 0)
(define main-tc '())

(define-values
  (main main-defaults)
  (lambda/params (samplerate timestep
                  voice_freq voice_gate ;; Driven by voice allocator
                  )

    (mix (v (nb_voices 16))
         ((freq voice_freq)
          (gate voice_gate))
         (let ((f (* freq timestep))
               (spread 0.01))
           (* gate 0.1 (supersaw f spread))))))
