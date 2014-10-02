#lang racket/base
(require rai/tools
         rai/ai-taylor
         rai/ai-symbolic
         rai/stream-syntax
         rai/stream-lib
         rai/stream-meta
         "test-lang.rkt")


(define tx-tailor (ai-taylor test-square))

(define series ((ai-symbolic tx-tailor) (ai-tailor-symbolic 'a)))

(for ((i (in-range 5)))
  (pretty-print (series i)))
