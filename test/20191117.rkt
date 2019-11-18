#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))

;; See doodle.rkt
;; This is messy exploratory stuff.
;; When cloning, add it to 

(define-values
  (main main-defaults)
  (lambda/params (samplerate)
    (let* ((f     '0.0017)
           (tick  (float (timer '4700.0)))    ;; 3500 - 6100 
           (tick1 (float (timer '4500.0)))    ;; 3500 - 6100 
           (e1    (env-AR tick '0.46 '0.0013)) ;; .01 - .0003  
           (e2    (env-AR tick1 '1.0 '0.0019)) ;; .00052 - .01
           (s     (* .1 (* e1 (saw-d3 f))))
           (l     (svf-lp s (* e2 '120.0 f) '0.069))
           )
      
      ;; (fdn4 x (vector 113 227 1397 11101.0))

      ;; (megasaw6)
      ;; (tinpan)
      
      (* '0.91 l)
      
      )))

(define main-nsi 0)
(define main-tc '())

