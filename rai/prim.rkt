#lang racket/base
; (require racket/math)
(provide (all-defined-out))

;; Primitives for the ai-scheme interpretation.

;; Pure functional
(define (p_add a b) (+ a b))
(define (p_sub a b) (- a b))
(define (p_mul a b) (* a b))

(define (p_and a b) (bitwise-and a b))
(define (p_or  a b) (bitwise-ior a b))
(define (p_xor a b) (bitwise-xor a b))


(define (p_div a b)
  ;; Allow for division by zero.
  (if (zero? b)
      +inf.0
      (/ a b)))
(define (p_pow a b) (expt a b))


(define (p_mod a b) (remainder a b))

(define (p_floor x) (floor x))

(define (p_lt a b c d) (if (< a b) c d))


(define (p_exp x) (exp x))
(define (p_sin x) (sin x))
(define (p_cos x) (cos x))
(define (p_log x) (log x))

(define (p_atan x) (atan x))

(define (p_copy x) x)

(define (p_debug x)
  (display (format "~a\n" x) (current-error-port))
  x)


