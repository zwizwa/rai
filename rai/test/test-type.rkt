#lang racket/base
(require "tools.rkt"
         "type.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         "test-lang.rkt")

(define t1 (make-type 'Array 10 (list (make-base-type 'Float))))
(define t2 (make-type 'Array 10 (list (make-base-type 'Float))))
(define t3 (make-type 'Array  9 (list (make-base-type 'Float))))

(type-constructor-equal? t1 t1)
(type-constructor-equal? t1 t2)
(type-constructor-equal? t1 t3)

(define v1 (make-type-var 'v1))
(define t4 (make-type 'Array 9 (list v1)))

(occurs-check v1 t1)
(with-handlers ((void display))
  (occurs-check v1 t4))
(display " OK\n")

;; Easier for debugging
(type-environment (make-bound-id-table))
(node-gensym      (make-gensym (prefix "v")))


(define r1 #'r1) (typed-node! r1)
(define r2 #'r2) (typed-node! r2)

(dict-ref (type-environment) #'r1)
(dict-ref (type-environment) #'r2)

(->datum (dict->list (type-environment)))

(unify-expr! (list #'r1 #'r2))

(types)


(define t5 (make-type 'Array 8 (list t3)))
(type-base t5)

  
(type-index-list t5)

(unify! (make-base-type 'Float) (make-type-var 'r2))
(types)

;; (ai-array test-delay)
;; (ai-array test-integrate-2D)

