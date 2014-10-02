#lang racket/base
(require "tools.rkt"
         "ai-taylor.rkt"
         "ai-symbolic.rkt"
         "stream-syntax.rkt"
         "stream-lib.rkt"
         "stream-meta.rkt"
         "test-lang.rkt")


(define tx-tailor (ai-taylor test-square))

(define series ((ai-symbolic tx-tailor) (ai-tailor-symbolic 'a)))

(for ((i (in-range 5)))
  (pretty-print (series i)))
