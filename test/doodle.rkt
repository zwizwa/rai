#lang s-exp rai/stream
(require rai/stream-lib
         rai/synth-lib)

(provide (all-defined-out))

(define main-nsi 0)
(define main-tc '())


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


(define (main samplerate)
  (megasaw6)
  ;;(test6)
  )


  
