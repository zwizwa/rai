#lang racket/base
(require rai/tools
         rai/ai-eval
         "test-lang.rkt")

(define (test desired approx input
              [epsilon 0.001]
              )
  (printf "~a\n" desired)
  (for ((i input))
    (let* ((i (exact->inexact i))
           (e (desired i))
           (a ((ai-eval approx) i))
           (rele (/ (- e a) e)))
      (printf "~a:\t~a\t~a\t~a\n" i e a rele)
      (when (> (abs rele) epsilon)
        (error 'approx-error (format "~s" rele)))
      )))

(test exp test-t-exp-6 '(.001 .01 .1 1))
(test sin test-t-sin-6 '(.001 .01 .1 1 1.8))
(test cos test-t-cos-6 '(.001 .01 .1 1 1.8))



