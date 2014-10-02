#lang racket/base
(require "tools.rkt"
         "type.rkt"
         "ai-array.rkt")
(provide (all-defined-out))

;; Scheme implementation for the ai-array intermediate language output.

(define (ai-array/scheme program)
  (void))

;; Compiler driver.
(define (ai-array-scheme program
                         #:nsi (nsi 0))   ;; Default: no inputs are streams
  (ai-array/scheme
   (ai-array program)))
