#lang racket/base
(require
 "tools.rkt"
 "stream-syntax.rkt" ;; base language core syntax
 "type.rkt"          ;; type unification
)

(provide ai-annotate)

;; Generic annotation.

;; This is a semantics combinator.  Two semantics at play: one
;; generates nodes, and the other expects node-annotated values.


;; Trouble:
;;  - higher order functions?
;;  - output types?
