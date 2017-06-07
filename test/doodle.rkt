#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))

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
;; doodle.lc2" in this directory, and loading "emacs/rai.el" into
;; Emacs.

(define-values
  (main main-defaults)
  (lambda/params (samplerate)
    (let* ((f    '0.23) 
           (tick (float (timer '9100.0)))    ;; 3500 - 6100 
           (e1   (env-AR tick '0.42 '0.014)) ;; .01 - .0003  
           (e2   (env-AR tick '1 '0.0097)) ;; .00052 - .01
           (s    (* .1 (* e1 (saw-d3 f))))
           (l    (svf-lp s (* e2 '16.0 f) '0.031))
           )
      (* '0.1 l)
      ;;(fdn1)
      )))

(define main-nsi 0)
(define main-tc '())

;; TODO:
;; - keep on adding LFOs
;; - add a simpler exampe, then separate out "performances"
;; - use hamiltonian modulators


;; (define (phasor32 (s) (period32))
;;   (let* ((s_next (+ s period32))
;;          ;(dummy1 (int 0))
;;          ;(dummy2 (+ s_next dummy1))
;;          (scale (/ 1 #x7FFFFFFF)))
;;     (values s_next
;;             (* scale (float s)))))


(define (megasaw1)
  (let ((f .001))
    (mix (v (nb_osc 1000))
         ()
         (* 0.001 (saw-d3
                   (* f (+ 1 (* 0.001 (float v))))
                   )))))

(define (megasaw2)
  (let ((f .001))
    (mix (v (nb_osc 1000))
         ()
         (let* ((v1 (* 0.001 (float v)))
                (v2 (* v1 v1)))
           (* 0.0001 (saw-d3
                      (* f (+ 1 v1 v2))
                      ))))))

(define (megasaw3)
  (let ((f .001))
    (mix2 (v (nb_osc 500))
         ()
         (let* ((v1 (* 0.001 (float v)))
                (v2 (* v1 v1))
                (v3 (* 0.0011 (float v)))
                (v4 (* v3 v3))
                (g 0.0001)
                )
           (values
            (* g (saw-d3 (* f (+ 1 v1 v2))))
            (* g (saw-d3 (* f (+ 1 v3 v4)))))))))

;; Saw decorrelation over time.
;; Might be interesting to run this in reverse!
(define (megasaw4)
  (let ((f .001))
    (mix2 (v (nb_osc 50))
         ()
         (let* ((v1 (* 0.001 (float v)))
                (v2 (* v1 v1))
                (v3 (* 0.0011 (float v)))
                (v4 (* v3 v3))
                (g 0.0005)
                )
           (values
            (* g (saw-d3 (* f (+ 1 v1 v2))))
            (* g (saw-d3 (* f (+ 1 v3 v4)))))))))

;(define (main samplerate)
;  (megasaw3))

;; (define (main samplerate)
;;   (loop (c (nb_channels 2))
;;         ()
;;         ()
;;         (float c)))


(define (megasaw5)
  (let ((f 0.002))
    (loop (c (nb_channels 2))
          ()
          ()
          (saw-d1 (* f (+ 1 (* 0.01 (float c))))))))

(define (megasaw6)
  (let ((f .001))
    (loop (c (nb_channels 2))
          ()
          ()
          (mix (v (nb_osc 50))
               ()
               (let* ((v1 (* 0.001 (float v)))
                      (v2 (* v1 v1))
                      (v3 (* 0.02 (float c)))
                      (g 0.0005)
                      )
                 (* g (saw-d3 (* f (+ 1 v1 v2 v3)))))))))

(define (test6)
  (loop (c (nb_channels 2))
        ()
        ()
        (mix (v (nb_osc 50))
             ()
             (float v))))

(define (del1)
  (let* ((x_i (timer 25000))
         (x (float x_i)))
    (delay/fixed-fb x 8123 0.5)))

(define (tinpan)
  (let* ((x_i (timer 25000))
         (x (float x_i)))
    (fdn4 x (vector 13 27 97 101))))

(define (fdn1)
  (let* ((x_i (timer 25000))
         (x (float x_i)))
    (fdn4 x (vector 113 227 397 1100.0))))






  
