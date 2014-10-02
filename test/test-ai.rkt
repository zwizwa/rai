#lang racket/base
(require
 rai/tools
 rai/stream-lib
 rai/stream-syntax
 rai/ai-array
 rai/ai-autodiff
 rai/ai-eval
 "test-lang.rkt")

;; Simple evaluator
((ai-eval test-add) 1 2)

;; Scheme syntax compiler
(pretty-print (ai-array test-program-1))




