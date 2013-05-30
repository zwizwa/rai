#lang racket/base
(require "tools.rkt"
         "prim.rkt"
         "stream-syntax.rkt")
(provide ai-grid)


;; Reduce program to flat operation on grids.


(define (ai-grid program parent-semantics)

  (define (feedback/n sem state-names
                      in-nodes time-names
                      update)
    #f)
  (define (reduce/n sem T reduce init body)
    #f)

  (define (lift op)
    (lambda grids
      (apply (ai-function-proc op)
             parent-semantics
             grids)))
  
  (define semantics
    (make-ai #:feedback/n  feedback/n
             #:reduce/n    reduce/n
             #:add         (lift ai-add)))
  (with-type-environment
   (lambda ()
     #f))
  )

             
