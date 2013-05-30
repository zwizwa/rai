#lang racket/base
(require "tools.rkt")
(provide (all-defined-out))


;; Typing context is stored in a parameter.  In most applications, it
;; acts as something "fairly global".  This allows most utility
;; functions to go into this file.

;; Same for node generator.  Just synchronize the gensym with the type
;; env construction.

;(define type-environment (make-parameter #f))
;(define node-gensym (make-parameter #f))


(define type-environment (make-parameter #f))
(define node-gensym (make-parameter #f))

(define (with-type-environment thunk)
  (parameterize ((type-environment (make-hasheq))
                 (node-gensym (make-gensym (prefix "r"))))
    (thunk)))


;; The types are currently restricted to kind * -> * (linear instead
;; of tree, e.g. arrays of arrays of ... base type).

;; Type constructors are integer-indexed, e.g. Array n where n is a
;; type index representing the array type.

;; For the unification algorithm, the important parts are:
;;    - compare constructors: both name and index should be the same
;;    - recurse down the type tree (list).

(define-struct type (name index params)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (lambda a (apply type-printer a)))])

(define-struct type-var (name)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (lambda a (apply type-var-printer a)))])


(define (type->sexpr t)
  (match t
    ((struct type (name #f '()))
     name)
    ((struct type (name index subs))
     (list name index (map type->sexpr subs)))
    (else t))) ;; e.g. type-var

(define (type-printer t port mode)
  (when mode (write-string "#<" port))
  (write-string (format "type:~a" (type->sexpr t)) port)
  (when mode (write-string ">" port)))


(define (type-var-printer v port mode)
  (when mode (write-string "#<" port))
  (write-string (format "type-var:~a" (type-var-name v)) port)
  (when mode (write-string ">" port)))


(define (make-base-type name) (make-type name #f '()))

(define Int       (make-base-type 'Int))
(define Float     (make-base-type 'Float))
(define Undefined (make-base-type 'Undefined))

;; Type constructor.
(define-syntax-rule (:: ctor index)
  (lambda type-params
    (make-type 'ctor index type-params)))

(define type-constructor-equal?
  (match-lambda*
   ((list (struct type (ctor1 i1 p1))
          (struct type (ctor2 i2 p2)))
    (and (eq? ctor1 ctor2)
         (or (not (or i1 i2)) ;; both #f -> not indexed type
             (eq? i1 i2))     ;; FIXME: can be number or symbol (type parameter)
         (= (length p1)
            (length p2))))
   (t (error 'type-constructor-equal (format "~a" t)))))


;; Register node identifier in type environment as a type variable.
(define (typed-node! node)
  (let* ((env (type-environment))
         ;; Name needs to be unique for type environment.  Use the
         ;; unmodified node name for easy reference to the original
         ;; node, but postfix it with an index in case it is not
         ;; unique.
         (tvar-name
          (if (dict-ref env node false)
              (string->symbol (format "~a:~a" (->datum node) (dict-count env)))
              (->datum node)))

         (tvar (make-type-var tvar-name)))
    (dict-set! env node tvar)))



;; Check for recursive types.
(define (occurs-check tvar texpr)
  (unless (type-var texpr)
    (let down ((te texpr))
      (match te
        ((struct type (name index params))
         (for ((p params)) (down p)))
        ((struct type-var (name))
         (when (eq? (type-var-name tvar) name)
           (error 'recursive-type (format "~a" texpr))))))))

;; Abstract map! over all type variables in type env.
(define ((map-texp fn) type-expr)
  (let recurse-te ((te type-expr))
    (match te
      ((struct type (cons index params))
       (make-type cons index
                  (for/list ((p params)) (recurse-te p))))
      ((struct type-var (name))
       (fn te)))))

(define (update-vars! fn [env (type-environment)])
  (for ((var (dict-keys env)))
    (dict-update! env var (map-texp fn))))
    
;; Once a type variable -> type expression binding is known, it can
;; be substituted in the type environment (type-environment).
(define (unify-var! tvar texpr)
  (unless (id=? tvar texpr)
    (occurs-check tvar texpr)
    (update-vars!
     (lambda (tv)
       (if (eq? (type-var-name tv)
                (type-var-name tvar))
           texpr
           tv)))))

;; Recursive destructuring up to unification of variables.
(define (unify-2! t1 t2)
  (match (list t1 t2)
    ((list (struct type-var (n1)) _) (unify-var! t1 t2))
    ((list _ (struct type-var (n2))) (unify-var! t2 t1))
    ((list (struct type (c1 i1 ps1))
           (struct type (c2 i2 ps2)))
     ;; FIXME: Also infer constructors?
     (unless (type-constructor-equal? t1 t2)
       (error 'type-error (format "~a\n~a" (list t1 t2) (types))))
     (for ((p1 ps1) (p2 ps2)) (unify! p1 p2)))))

;; List of types: perform cross product.
(define (unify! t1 . ts)
  ;; (stderr-pp (cons t1 ts))
  (unless (null? ts)
    (unify-2! t1 (car ts))
    (apply unify! ts)))


(define (node-type n)
  (dict-ref (type-environment) n
            ;; FIXME: This is to support lists of nodes.  Maybe all
            ;; typed nodes should be created on-demand?
            (lambda _
              (typed-node! n)
              (node-type n))))




;; Return type environment with cleaned up type names in OCaml style:
;; a .. z a1 .. z1 a2 ...;(test test-delay)


;; (define (copy-env [te (type-environment)])
;;   (make-bound-id-table (dict->list te)))

;; (define (types)
;;   (define *t-copy* (copy-env))
  
;;   (define (unique-vars)
;;     (define *tvs* (make-bound-id-table))
;;     (update-vars!
;;      (lambda (id) (dict-set! *tvs* id #t) id)
;;      *t-copy*)
;;     (dict-keys *tvs*))
  
;;   ;; Replace syms.  The renaming won't clash because of the prefixes
;;   ;; in the (type-environment) table.
;;   (for ((v (unique-vars))
;;         (n (in-naturals)))
;;     (update-vars!
;;      (lambda (id)
;;        (if (id=? id v)
;;            (ocaml-type-symbol n)
;;            id))
;;      *t-copy*))
  
;;   (dict->list (type-environment)))

(define (types) (dict->list (type-environment)))





;; Some tools.

;; Unify types in a mixed list of nodes and literals.
(define (unify-expr! expr)
  (define (valid-node? x)
    (not (number? x)))
  (apply unify!
         (map node-type
              (filter valid-node? expr))))
;; Type unification accross accumulation rank n+1 -> rank n
(define (unify-cross-rank! T element-node struct-node)
  (let ((te (node-type element-node))
        (ts (node-type struct-node)))
    ;; (stderr-pp (list te ts (T te)))
    (unify! ts (T te))))

;; Each node has an associated type expression, initialized to a type
;; variable.
(define (make-node)
  (let ((node ((node-gensym))))
    (typed-node! node) node))
(define (make-nodes n)
  (for/list ((i (in-range n))) (make-node)))

(define (type-base t)
  (match t
    ((struct type (_ _ (list p)))
     (type-base p))
    ((struct type (_ _ '()))
     t)
    ((struct type-var (_))
     t)
    (else
     (begin
       ;; FIXME: Hack to bridge proper Int typing implementation.
       ;; (stderr (printf "WARNING: type-base: ~a\n" t))
       t))))


;; Convert 1D indexed type constructors to a list of indices
;; ex: (Array A (Array B (Array C (Float)))) -> (A B C).

;; Note that since we are only using arrays as basic types, all the
;; index tags are array sizes.  The constructors just differentiate
;; between different use cases of arrays.  Currently 'Array and
;; 'Delay.  Later probably also lookup tables.

(define (type-index-list t [tail (lambda _ '())])
  (let sub ((t t))
    (match t
      ((struct type (ctor #f '()))
       (tail))
      ((struct type-var (_))
       (tail))
      ((struct type (_ index (list p)))
       (cons index (sub p)))
      (else
       (begin
         ;; FIXME: Hack to bridge proper Int typing implementation.
         (stderr (printf "WARNING: type-index-list: ~a\n" t))
         (tail))))))

;; FIXME: This is currently only checked for 'Array, but probably also
;; works for 'Delay.
(define (type-vector-size type type-index-mapping)
  (let ((index (type-index type)))
    (or (and (number? index) index)
        (dict-ref type-index-mapping index))))

