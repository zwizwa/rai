#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))
(define main-nsi 0)
(define main-tc '())

;; A simple form of "live coding", not necessarily aimed at
;; performance, but at making it more convenient to perform parameter
;; adjustments whenbuilding a DSP effect or synthesis module.  It is
;; based on two priciples:
;;
;; - Number edits in the source can be linked to live parameter
;;   updates in the running code without recompiling anything.
;;
;; - Structural changes in the source needs a recompile, but starting
;;   compile and reload can be automated, triggerd by source file save.
;;
;; The aim is to make the edit-to-ear cycle short, e.g. not interrupt
;; the running effect/synth for a long time, or not at all in the case
;; of simple number changes.
;;
;;
;; To make this more convenient (in Emacs):
;;
;; - To avoid most manual number edits, M-up / M-down perform relative
;;   adjustments to the number at point.  If the number is registered
;;   as a parameter, its updated value is sent to the running code.
;;
;; - In order to support the above, code needs to be annotated to
;;   indicate which magic constants present in the source should be
;;   (temporarily) treated as parameters.  Standard lisp quote ticks
;;   are used as a marker.  The scheme form lambda/params will gather
;;   all tick-marked constants in a and will compile them into the
;;   binary as paramters that can be set using Pd-style messages, and
;;   corresponding Emacs code can walk the code in a lambda/params
;;   form to associate number locations to parameter names understood
;;   by the binary.
;;
;; For this file, the workflow above can be accessed by typing "make
;; doodle.lc" in this directory, and loading "emacs/rai.el" into
;; Emacs.

;; Aliasing supersaw.
(define (ssaw freq spread n)
  (mix (i (nb_saws n))
       ()
       (let* ((i   (cast Float i))
              (n   (cast Float nb_saws))
              (dx  (- (* 2 (/ i n)) 1))
              (cube (* dx dx dx)))
         (phasor (* freq (+ 1 (* spread cube)))
                 -1 1))))


(define-values
  (main main-defaults)
  (lambda/params
   (samplerate)
   (let* ((ff (+ '0.063 (* '0.011 (phasor '9.1e-05 1 0))))
          (s1 (ssaw '0.004 '0.0023 10))
          (s2 (ssaw '0.006 '0.039 10))
          (s (+ s1 s2))
          (l (svf-lp (* '0.032 s) ff '0.01)))
     (* '0.47 l))))

