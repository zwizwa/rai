#lang scheme/base
(require ;"ai-ssa.rkt"
         ;"tools.rkt"
         "stream-syntax.rkt")
;(pretty-print
; (->datum
;  (ai-scheme-stx
   (stream
    (lambda (a b)
      (add a (add a b))))
;)))
;(stream-lambda (a b) (add a (add a b)))
