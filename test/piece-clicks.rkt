#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)

(provide (all-defined-out))
(define main-nsi 0)
(define main-tc '())
(define-values
  (main main-defaults)
  (lambda/params (samplerate)
    (let* ((f    '0.0013) ;; 0.0008 - 0.0035
           (tick (float (timer '5700.0))) ;; 3500 - 6100 
           (e1   (env-AR tick 1 '0.0013)) ;; .01 - .0003  
           (e2   (env-AR tick 1 '0.00022)) ;; .00052 - .01
           (s    (* .1 (* e1 (saw-d2 f))))
           (l    (svf-lp s
                         (* e2 '8.3 f)  ;; 5 - 400
                         '0.017)) ;; 1.0  - 0.1
           )
      (sat (* '4.3 l))  ;; 1 - 50
      )))

;; - What are the units?
;; - How to make a logarithmic LFO?
;; - What is creating the modulation?  (beating of metronome vs. freq chopped by ar)

;; Making synths is distilling units.

  
