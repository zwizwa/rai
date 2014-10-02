#lang racket/base
(require "tools.rkt"
         "prim.rkt"
         "stream-syntax.rkt"
         racklog)
(provide ai-linpar)

;; Type inference for linear functions: split nodes into linear
;; variable (L) and multiplicative constant parameter (P) partitions.


;; List versions of %and and %or
(define-syntax-rule (%and* gs) (for/fold ((a %true))  ((g gs)) (%and g a)))
(define-syntax-rule (%or*  gs) (for/fold ((a %false)) ((g gs)) (%or  g a)))


;; Basic operations constrain node types.

(define (constr-add c a b)
  (%and (%= c a)
        (%= c b)))

(define (constr-mul . ts)
  (%or (%= ts '(L L P))
       (%= ts '(L P L))
       (%= ts '(P P P))))

(define (constr-div . ts)
  (%or (%= ts '(P P P))
       (%= ts '(L L P))))


;; Nonlinear nodes do not participate in the analysis.
(define (constr-same . ts)
  (define Ps (map (lambda _ 'P) ts))
  (define Ls (map (lambda _ 'L) ts))
  (%or (%= ts Ps)
       (%= ts Ls)))

;; OC is not necessary; the input program is a DAG.
;; (use-occurs-check? #t)

(define (ai-linpar program
                   [in-types '()]
                   [out-types '()])

  (define nodes '())
  (define constraints '())

  (define (make-node)
    (push! nodes (_))
    (car nodes))
  
  ;; FIXME: Make sure literals expand to (ai-literal) in
  ;; stream-syntax.rkt then remove this hack.
  (define (annotate-number n)
    (if (number? n)
        (let ((n (make-node)))
          (push! constraints (constr-same n))
          n)
        n))
  
  (define (prim constr)
    (lambda (sem . in)
      (let* ((in (map annotate-number in))
             (out (make-node)))
        (push! constraints (apply constr out in))
        out)))

  (define semantics
    (make-ai #:add (prim constr-add)
             #:sub (prim constr-add)
             #:mul (prim constr-mul)
             #:div (prim constr-div)
             ;; #:cmp (prim constr-same)
             ))

  (define (i/o-constraints nodes types)
    (for/fold ((lst '())) ((n nodes) (t types))
      (case t
        ((L P) (cons (%= n t) lst))
        (else lst))))

  (let* ((nb-in
          (ai-function-arity program))
         (in-nodes
          (for/list ((_ (in-range nb-in))) (make-node)))
         ;; Abstract-interpret program, registers nodes and
         ;; constraints as a side-effect.
         (out-nodes
          (values-list
           (apply (ai-function-proc program)
                  semantics in-nodes)))
         ;; Construct clauses for generate and test.
         (clauses
          (%and
           (%and* (for/list ((v nodes)) (%member v '(L P))))
           (%and* constraints)
           (%and* (i/o-constraints in-nodes  in-types))
           (%and* (i/o-constraints out-nodes out-types))
           ))
           
         ;; Run logic program
         (results     
          (%find-all (in out)
           (%and clauses
                 (%= in  in-nodes)
                 (%= out out-nodes)))))
    results))



