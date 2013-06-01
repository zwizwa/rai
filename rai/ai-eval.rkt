#lang racket/base
(require "tools.rkt"
         "prim.rkt"
         "stream-syntax.rkt")
(provide ai-eval ai-eval-semantics)

(define (prim op)
  (lambda (sem . args)
    (apply op args)))
(define ai-eval-semantics
  (make-ai #:default (lambda (sym op)
                       (lambda _
                         (error 'ai-eval:not-implemented
                                (format "~s" sym))))
           #:literal (prim (lambda (x) x))
           #:add     (prim p_add)
           #:sub     (prim p_sub)
           #:mul     (prim p_mul)
           #:div     (prim p_div)
           #:pow     (prim p_pow)
           #:floor   (prim p_floor)
           #:lt      (prim p_lt)
           #:if      (prim p_if)
           #:exp     (prim p_exp)
           #:sin     (prim p_sin)
           #:cos     (prim p_cos)
           #:log     (prim p_log)
           #:atan    (prim p_atan)
           ))


;; Simple evaluation, with vector lifting, without stream semantics.

(define (ai-eval program)
  (lambda in
    (apply (ai-function-proc program)
           ai-eval-semantics in)))
