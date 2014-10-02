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


;; Compilation to completely flat code and data structure.  Optimize
;; later (through analysis of that structure?)




;; extend environment
(define (extend env name val)
  (cons (cons name val) env))
(define (init-env) '())
(define (ref env var)
  (dict-ref env var))
  

;; Sequence commands, binding results
(define (extend/b env bindings compile)
  (for/fold
      ((env env))
      ((b bindings))
    (match b
      ((list var expr)
       (extend env var (compile env expr))))))

(define (yield env var)
  (pretty-print env)
  (ref env var))

(define (run env expr)
  (match expr
    ((list 'let*     bindings body)
     (run (extend/b env bindings run) body))
    ((list 'let name bindings body)
     (run (extend env name (list 'fn (map car bindings) body))
          `(let* ,bindings ,body)))
    ((list 'yield  var)  (yield env var))
    ((list 'add1   var)  (add1 (ref env var)))
    ((list 'lit    l) l)
    ((list-rest 'app fn args)
     (map (lambda (v) (ref env v)) args)) ;; FIXME: tail call
    (else (error expr))))


;; (run '() program)


;; Tree-traversal code like this is easier to write with global
;; accumulators in dynamic parameters.

(define code-buffer  (make-parameter '()))
(define node-counter (make-parameter 0))

(define (code . exprs)
  (for ((e exprs))
    (code-buffer (cons e (code-buffer)))))
(define (n->node n)
  (string->symbol (format "n~s" n)))
(define (make-node)
  (let ((n (node-counter)))
    (node-counter (add1 n))
    (n->node n)))


(define (compile env   ;; compile-time environment
                 expr) ;; scheme-like expression
    (match expr
      ((list 'let name bindings body)
       ;; 1. Compile initialization expressions, capturing variable nodes
       (let*-values
           (((env nodes)
             (for/fold
                 ((env env)
                  (nodes '()))
                 ((b bindings))
               (match b
                 ((list var expr)
                  (let* ((node (compile env expr))
                         (env  (extend env var node)))
                    (values env
                            (cons node nodes)))))))
            ;; 2. Record jump label + variable nodes (for compiling function call)
            ((env) (extend env name nodes)))
         ;; 3. Compile label
         (code `(label ,name))
         ;; 4. Compile body
         (compile env body)))
      ((list 'let*     bindings body)
       (compile (extend/b env bindings compile) body))
      ((list 'prim fn var)
       (let ((n (make-node)))
         (code `(def ,n (,fn ,(ref env var))))
         n))
      ((list 'lit val)
       (let ((n (make-node)))
         (code `(def ,n ,val))
         n))
      ((list-rest 'app fn args)
       (let ((vars (ref env fn)))
         (code `(set-values! ,vars ,(for/list ((a args)) (ref env a))))
         ;; Only "yield" exit, no return value since program is a loop
         (code `(goto ,fn))
         (void)))
      (else (error expr))))


(define (compile-program p)
  (parameterize ((code-buffer '())
                 (node-counter 0))
    (compile '() p)
    (display "code:\n")
    (for ((c (reverse (code-buffer))))
      (pretty-print c))))

(compile-program p1)

(define p1
  '(let next ((a (lit 123)))
     (let* ((a (prim add1 a)))
       (app next a))))
