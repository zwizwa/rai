#lang scheme

;; Simple example of abstract interpretation of Scheme code.

;; Abstract syntax takes the form of a function that translates
;; semantics (The meaning of DSL's primitive functions `+' and `*')
;; into a function that can be evaluated over a certain domain (set of
;; values / types).
     
(define (program + *)
  (lambda (x y)
    (+ (* x x) (* y y))))

;; Evaluates to a function
(define (ai-eval abstract-syntax)
  (define (e-add a b) (+ a b))
  (define (e-mul a b) (* a b))
  (define interpretation (abstract-syntax e-add e-mul))
  interpretation)

;; Evaluates to a SSA form
(define (make-ssa-dict nodes)
  (define count -1)
  (define (add-node! expr)
    (set! count (add1 count))
    (let ((node (string->symbol (format "v~s" count))))
      (set! nodes (cons (list node expr) nodes))
      node))
  (define (node! expr)
    (if (symbol? expr)
        expr
        (add-node! expr)))
  (define (bindings) (reverse nodes))
  (values node! bindings))

(define (ai-code abstract-syntax)
 (let-values (((node! bindings)
               (make-ssa-dict '())))
   (define ((e-op op) a b)
     (node! `(,op ,a ,b)))
   (define args '(a b))
   (let* ((interpretation
           (abstract-syntax (e-op '+)
                            (e-op '*)))
          (rv (apply interpretation args)))
     `(lambda ,args
        (let* ,(bindings) ,rv)))))


(define test-eval ((ai-eval program) 1 2))
;; => 5

(define test-code (ai-code program))
;; => '(let ((v0 (- a a)) (v1 (- b b)) (v2 (+ v0 v1))) v2)


;; The code in meta/rai expands on this by:
;;
;; - Using a thin layer of syntactic abstractions to hide semantics
;;   threading (passed as first argument to each function)
;;
;; - Compilation to Scheme syntax and C code (proper homogeneous and
;;   heterogeneous multi-stage programming)
;;
;; - More specific DSL features (causal stream processors)

