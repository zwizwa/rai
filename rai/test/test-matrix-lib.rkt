#lang racket/base
(require "matrix-lib.rkt"
         "ai-eval.rkt")

(define X '((1 1)
            (0 1)))
(define XX (ai-matrix-mul ai-eval-semantics X X))

(define A '((1 2)
            (3 4)))

(define iA (ai-matrix-inverse ai-eval-semantics A))
(define one (ai-matrix-mul ai-eval-semantics A iA))