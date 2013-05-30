#lang racket/base
(require
 "tools.rkt"
 "stream-lib.rkt"
 "stream-syntax.rkt"
 "ai-array.rkt"
 "ai-autodiff.rkt"
 "ai-eval.rkt"
 "test-lang.rkt")

;; Simple evaluator
((ai-eval test-add) 1 2)

;; Scheme syntax compiler
(pretty-print (ai-array test-program-1))




