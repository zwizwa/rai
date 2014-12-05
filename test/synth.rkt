#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)

(provide (all-defined-out))


;; Example Audio Synthesizer.

;; Number of stream inputs.
(define main-nsi 0)
(define main-defaults '())

(define main
  (lambda (voice_freq voice_gate ;; Driven by voice allocator
           timestep samplerate
           spread
           attack decay sustain release
           cutoff
           filter_attack
           filter_decay
           filter_env
           filter_q
           )
    
    (let* ((1/fs timestep)
           (fs   samplerate)
           (pi   (* 4 (atan 1)))
           ;; UI input specs.  Output is normalized units.
           (a (p-rate-ms 1 attack  "Attack"  100    fs))
           (d (p-rate-ms 1 decay   "Decay"   100    fs))
           (s (p-gain-db 1 sustain "Sustain" -20 0    ))
           (r (p-rate-ms 1 release "Release" 100    fs))
           ;; Filter
           (c (p-freq-Hz/i 1 cutoff  "Cutoff"  20 5000 (* 2 pi 1/fs)))
           (f-a (p-rate-ms 1 filter_attack  "Filter Attack"   100  fs))
           (f-r (p-rate-ms 1 filter_decay   "Filter Release"  100  fs))
           (f-e (p-freq-Hz/i 1 filter_env   "Filter Env"  20 5000 (* 2 pi 1/fs)))
           (f-q (hold (/ 1 (p-gain-db 1 filter_q      "Filter Q"    0 30))))
           ;; Oscillator
           (spread (p-percent 1 spread "Osc Spread"))  ;; FIXME: scale isn't right: use a squashed log
           )
      (mix (v (nb_voices 32))
           ((freq voice_freq)
            (gate voice_gate))
           (svf-lp
            (* (env-ADSR gate a d s r)
               (let ((f (* freq 1/fs)))
                 (* 0.1
                    (supersaw f spread))))
            (+ 0 (* c (env-AD gate f-a f-r)))
            f-q)))))
  
  

