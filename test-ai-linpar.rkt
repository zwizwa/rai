#lang racket/base
(require "tools.rkt"
         "ai-linpar.rkt"
         "test-lang.rkt")

(define (t . a)
  (let ((res (apply ai-linpar a)))
    (pretty-print res)
    res))

(define prog1 (t test-linpar1)) 
(define prog2 (t test-linpar2)) 
(define prog3 (t test-linpar2 '(L)))

