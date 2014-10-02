#lang s-exp rai/stream
(require rai/stream-lib)
(provide (all-defined-out))


;; Number of stream inputs.
(define main-nsi 0)

(define (main voice_freq voice_gate ;; Driven by voice allocator
              samplerate
              spread)
  (let* ((inv_sr (/ 1 samplerate)))
    (params
     1
     ((spread lin "Saw Spread" "Hz" 0 1000))

     (mix (v (nv 5))
          ((freq voice_freq)
           (gate voice_gate))
          (let* ((f (* inv_sr freq))
                 (s (* f spread)))
            (* gate
               (mix (o (no 5))
                    ()
                    (let* ((o  (::Float o))
                           (no (::Float no))
                           (dx  (- (* 2 (/ o no)) 1))
                           (cube (* dx dx dx)))
                      (phasor (+ f (* s cube)) -1 1)))))))))


;; (define (main voice_freq voice_gate ;; Driven by voice allocator
;;               samplerate
;;               spread)
;;   (let* ((inv_sr (/ 1 samplerate)))
;;     (params
;;      1
;;      ((spread lin "Saw Spread" "Hz" 0 1000))

;;      (for (Array 'nb_voices)
;;        (lambda (voice-bus freq gate)
;;          (let ((f (* inv_sr freq)))
;;            (+ voice-bus
;;               (* gate
;;                  (for (Array 'nb_osc)
;;                    (lambda (osc-bus)
;;                      (let ((s 1)
;;                            (cube 0)) ;; FIXME
;;                        (+ osc-bus
;;                           (phasor (+ f (* s cube))
;;                                   -1 1))))
;;                    (0)
;;                    ())))))
;;        (0)
;;        (voice_freq
;;         voice_gate)))))

         
     

     
     ;; (bus nb_voices
     ;;      ()
     ;;      (inv_sr spread)  ;; Constant over loop
     ;;      (let* ((f (* inv_sr voice_freq))
     ;;             (s (* f spread)))
     ;;        (* voice_gate
     ;;           (bus nb_osc
     ;;                (i n)
     ;;                (f s)
     ;;                (let* ((i (::Float i))
     ;;                       (n (::Float n))
     ;;                       (dx  (- (* 2 (/ i n)) 1))
     ;;                       (cube (* dx dx dx)))
     ;;                  (phasor (+ f (* s cube))
     ;;                          -1 1)))))))))

  
