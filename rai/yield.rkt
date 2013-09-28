#lang racket/base
(require racket/match
         racket/dict
         racket/pretty)

;; Stand-alone exploration of flat coroutine to FSM compiler.

;; Main problem is to represent environments and continuations, then
;; analyze the continuations to produce a proper representation.  A
;; CPS representation is probably best.

;; A simple example program: with 
;; - named let
;; - ordinary let*
;; - primitive invocation
;; - yield
;; - tail call
;;
;; without:
;; - non-tail non-primitive calls


(define program
  '(let next ((a (lit 0)))
     (let* ((a (add1 a))
            (_ (yield a)))
       (app next a))))

;; extend environment
(define (extend env name val)
  (cons (cons name val) env))
(define (init-env) '())
(define (ref env var)
  (dict-ref env var))
  

;; Sequence commands, binding results
(define (compile* bindings env)
  (for/fold
      ((env env))
      ((b bindings))
    (match b
      ((list var expr)
       (extend env var (compile expr env))))))

(define (print-yield env)
  (pretty-print env))


(define (compile p [env (init-env)])
  (match p
    ((list 'let name bindings body)
     (compile body (compile* bindings env)))
    ((list 'let* bindings body)     (compile body (compile* bindings env)))
    ((list 'yield  var)  (print-yield env) (void)) ;;
    ((list 'add1   var)  (add1 (ref env var)))
    ((list 'lit    l) l)
    ((list-rest 'app fn args)
     (map (lambda (v) (ref env v)) args)) ;; FIXME: tail call
    (else (error p))))


(compile program)
