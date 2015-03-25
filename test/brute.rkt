#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)
(provide (all-defined-out))
(define main-nsi 0)
(define main-tc '())

;; Bought a microbrute early this week.  Lovin' it, so using some of
;; that fresh romance to see if I can get this thing here booted up
;; again.  It all feels too heavyweight: there's no way to get into
;; this without thinking hard..  That's OK for fresh coffeed daytime
;; hacking, but nighttime tweaking doesn't work like that..

;; What I'n curious about:
;;  - Will virtual analog get as dirty as the brute?
;;  - How to properly tweak envelopes?
;;  - How to turn synth building into a discipline?
;;  - Why is the trim so hard if the principles are simple?


(define-values
  (main main-defaults)
  (lambda/params (samplerate)
    (let* ((f    '0.0035)
           (tick (float (timer '6100.0))) ;; 3500 - 6100 
           (e1   (env-AR tick 1 '0.00029)) ;; .01 - .0003  
           (e2   (env-AR tick 1 '0.0031)) ;; .00052 - .01
           (s    (* .1 (* e1 (saw-d2 f))))
           (l    (svf-lp s (* e2 '22.0 f) '1.1))
           )
      (* 0.15 l)
      ;; (fdn1)
      )))



  
