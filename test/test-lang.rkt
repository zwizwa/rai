#lang s-exp rai/stream
(require rai/stream-lib
         rai/type
         rai/stream-meta)
(provide (all-defined-out))

(define (test-program-1 x y)
  (let* ((sum (ai-add x y)))
    (values sum (z^-1 sum))))

(define (test-program-2 x y)
  (let*-values
      (((s i) (test-program-1 x y)))
    (test-program-1 s i)))
      
(define (integrate3 x)
  (integrate (integrate (integrate x))))


(define (level-test (s) (x))
  (values (integrate s) (integrate x)))

          
(define (deriv-x3 x) (* x x x))
(define (deriv-x2 x) (* x x))
(define (deriv-x1 x) x)

(define (test-peval x)
  (+ (* x 0) 1 2 3))


(define (test-pd inc min max)
  (phasor (* inc 0.05) min max))

(define (test-pd1 inc min max)
  (phasor (* inc 0.025) min max))

                  
(define (test-local-function x y)
  (let ((sq (lambda (x) (* x x))))
    (+ (sq x)
       (sq y))))

(define (nphasor f)
  (mix (i (n 3))
       ()  
       (phasor f -1 1)))

(define (test-integrate-2D x)
  (mix (i (ni 3)) ()
  (mix (j (nj 4)) ()
       (integrate x))))


(define (test-add a b) (+ a b))

(define (test-vector a b c)
  (let-values (((z y x) (unpack (vector a b c))))
    (values z y x)))

(define (test-linpar2 a b c)
  (+ (* a b) c))

(define (test-linpar1 a b)
  (* a b))

(define (test-z3 (s1 s2 s3) (i))
  (values i s1 s2 s3))

(define (test-avg x)
  (/ (+ x (z^-1 x)) 2))


(define (test-poly x)
  (poly x (vector 1 2 3 4)))

(define (test-t-exp-6 x) (t-exp x 6))
(define (test-t-sin-6 x) (t-sin x 6))
(define (test-t-cos-6 x) (t-cos x 6))

(define (test-square x) (* x x))

(define (test-autodiff-1 x)
  (exp (sin (cos (log (* x x))))))


(define (test-tailor-in x) (+ (* 2 x) 1))


(define (test-pow-10 x) (^ x 10))
(define (test-inv x) (^ x -1))
(define (test-sqrt x) (^ x 1/2))


(define (test-sat  x) (- x (* 1/3 x x x)))
(define (test-clip x) (clip x -1 1))

(define (test-svf (s1 s2) (i))
  (let ((_s1 (- s1 (* .1 (+ (* .1 s1) s2 i))))
        (_s2 (+ s2 (* .1 s1))))
    (values _s1 _s2 _s2)))

(define (test-svf-sat (s1 s2) (i))
  (let ((_s1 (test-sat (- s1 (* .1 (+ (* .1 s1) s2 i)))))
        (_s2 (test-sat (+ s2 (* .1 s1)))))
    (values _s1 _s2 _s2)))

(define (test-svf-clip (s1 s2) (i))
  (let ((_s1 (test-clip (- s1 (* .01 (+ (* .1 s1) s2 i)))))
        (_s2 (test-clip (+ s2 (* .01 s1)))))
    (values _s1 _s2 _s2)))




;; (define (test-dual-rate (s)
;;                         (i)
;;                         (t T)
;;                         (l))
;;   (let ((x (+ s i T)))
;;     (values x x))
;;   (let ((y (* s i l t)))
;;     (values y y)))


;; (define (test-dual-rate2 i)
;;   (let ((y (* i 2)))
;;     (values
;;      (test-dual-rate y)
;;      (* y y))))

(define (test-dead-code x)
  (let ((y (* x x)))
    x))





(define (test-subsample (s) (x))
  (let*
      ((s (setup (+ s 1) s))
       (y (hold x))
       (out (+ s x y)))
    (values out out)))


(define (test-hold x)
  (values (+ 100 x) (hold (+ 100 x))))

(define (test-setup (s) (x))
  (let* ((s (setup (+ 100 s) (+ 1 s))))
    (values s s)))


(define (test-setup-no-fb x)
  (setup (+ 100 x) x))

(define (test-hold-fb (s) (x))
  (values x
          (+ 100 x) (hold (+ 100 x))))


(define (test-time  () (x) (t T))  t)
(define (test-time2 () (x) (t T))  (+ 0 T))

(define (test-hold-inherit x)
  (let ((y (hold (* x x))))
    (+ y y)))
        
        





(define (test-undefined (s) (i))
  (values s i))

  
(define (test-tag x)
  (tag x '((name  . "Gain")
           (unit  . "dB")
           (scale . 1)      ;; input scale 0-1
           (curve . lin)    ;; Linear in dB!
           (min   . -60)
           (max   . 12))))


