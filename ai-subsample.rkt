#lang scheme/base
(require
 "tools.rkt"
 "stream-syntax.rkt" ;; base language core syntax
 "type.rkt")         ;; type unification

;; Given parent semantics, create two programs, one for setup, one for
;; update.  The resulting programs can be interpreted by single-rate
;; semantics (i.e. do not support a non-identity setup routine).

(define (ai-subsample program parent-semantics)
  (define (parent fn)
    (lambda (_ . args) (apply fn parent-semantics args)))

  (define (feedback/n sem
                      state-names
                      in-nodes
                      local-names
                      time-names
                      setup
                      update)
    ...)
  
  (define semantics
    (make-ai
     #:default    (parent ai-default)
     
     #:feedback/n feedback/n
     #:for/n      (parent ai-for/n)
     #:copy       (parent ai-copy)
     
     #:add        (parent ai-add)
     #:sub        (parent ai-sub)
     #:div        (parent ai-div)
     #:mul        (parent ai-mul)
     #:pow        (parent ai-pow)))
     
                        
  (with-type-environment
   (lambda ()
     (let* ((in-nodes  (make-nodes (length (ai-function-args program))))
            (out-nodes (apply (ai-function-proc program) semantics in-nodes)))
       ...))))


