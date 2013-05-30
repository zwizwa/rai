#lang racket/base
(require racket/match)
(provide (struct-out dual)
         number->real)
;; Collection of extended number structures.

;; http://en.wikipedia.org/wiki/Dual_numbers
(define-struct dual (x dx) #:transparent)

(define (number->real n)
  (match n
    ((struct dual (x dx))
     (if (zero? dx) x
         (error 'not-a-real-dual-number (format "~a" n))))
    (else
     (if (number? n) n
         (error 'not-a-number (format "~a" n))))))
         
