#lang s-exp "stream.rkt"
(require "compalg-lib.rkt"
         "stream-lib.rkt"
         "test-lang.rkt")

;; (define (test a b)  (+ (^ a 2) (^ b 2)))

(define (test x) (^ x 10))

test
(D test)
(D (D test))
