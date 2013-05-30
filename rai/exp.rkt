#lang racket/base

(define (run P0 P1 n #:exp [exp exp])
  (let* ((q  (exp (/ (- P1 P0) n)))
         (p0 (exp P0))
         (p1 (for/fold ((p p0))
                       ((i (in-range n)))
               (* p q))))
    (values p1 (exp P1))))

;; Evaluate approximation error for exp approximation.
(define (error P0 P1 n)
  (lambda (exp)
    (- (expt (exp (/ (- P1 P0) n)) n)
       (/ (exp P1) (exp P0)))))

;; exp Taylor series
(define (exp-taylor order [acc 0])
  (lambda (x)
    (call-with-values
        (lambda ()
          (for/fold ((acc acc)
                     (xk 1)
                     (kfac 1))
              ((k (in-range (add1 order))))
            (values
             (+ acc (/ xk kfac))
             (* xk x)
             (* kfac (add1 k)))))
      (lambda (acc . _) acc))))


;; Test  
(define e (error +1i +.9i 100))