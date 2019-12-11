#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))

;; See doodle.rkt
;; This is messy exploratory stuff.
;; When cloning, add it to 

;; Connect midi keyboard to m_tune
;; The rest should go to controller keys.

(define-values
  (main main-defaults)
  (lambda/params (timestep m_tune)
    (let* ((f     (p-log 127 m_tune 20 2000 timestep)) ;; Osc frequency
           (tick  (float (timer '1200.0)))    ;; 3500 - 6100 
           (tick1 (float (timer '1100.0)))    ;; 3500 - 6100 
           (e1    (env-AR tick '0.46 '0.0069)) ;; .01 - .0003  
           (e2    (env-AR tick1 '1.0 '0.0063)) ;; .00052 - .01
           (s     (* .1 (* e1 (saw-d3 f))))
           (l     (svf-lp s (* e2 '91.0 f) '0.015))
           )
      
      ;; (fdn4 x (vector 113 227 1397 11101.0))

      ;; (megasaw6)
      ;; (tinpan)
      
      (* '0.92 l)
      
      )))

(define main-nsi 0)
(define main-tc '())

