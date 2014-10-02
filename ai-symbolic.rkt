#lang racket/base

(require "stream-syntax.rkt"
         "peval.rkt"
         "prim.rkt"
         "tools.rkt"
         "ai-eval.rkt"
         )
(provide ai-symbolic)


;; Transform abstract syntax to tree formula, i.e. no node sharing.
;; This is for debugging, as the absence of sharing makes it less useful for code.

;; FIXME: It might be useful to add support a `let' form whenever an
;; expression is used more than once, otherwise inline in the
;; expression tree.

(define (pretty-symbolic form)
  (define (pretty-tag x)
    (case x
      ((p_add) '+)
      ((p_sub) '-)
      ((p_mul) '*)
      ((p_div) '/)
      ((p_pow) '^)
      (else x)))
  (match form
    ((list-rest tag args)
     (cons (pretty-tag tag)
           (map pretty-symbolic args)))
    (else form)))
     

(define (ai-symbolic program)
  (lambda args
    (define (literal sem x) x)
    (define (op sym)
      (lambda (sem . args) (cons sym args)))
    (define ((ni op) . _)
      (error 'ai-symbolic-not-inplemented (format "~a" op)))

    (define (prim sym fn)
      (lambda (sem . expr)
        (->datum (peval ai-eval-semantics fn sym expr))))

    (define ai-symbolic-semantics
      (make-ai #:default
               (lambda (sym fn)
                 (lambda (sem . args)
                   (cons sym args)))
               #:add (prim 'p_add p_add)
               #:sub (prim 'p_sub p_sub)
               #:mul (prim 'p_mul p_mul)
               #:div (prim 'p_div p_div)
               #:pow (prim 'p_pow p_pow)
               #:literal literal
               #:for/n      (ni 'reduce/n)
               #:feedback/n (ni 'feedback/n)))
    (pretty-symbolic
     (apply (ai-function-proc program)
            ai-symbolic-semantics
            args))))
