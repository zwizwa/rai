#lang racket/base
(require
 "tools.rkt"
 "stream-syntax.rkt" ;; base language core syntax
 "type.rkt")         ;; type unification


;; Perform SSA structural analysis and typing, but keep the low-level
;; semantics parameterized.

(define (ai-annotated program)

  (define nodes    '())
  (define state-out '())
  (define state-in  '())

  (define loop     (make-parameter '()))
  (define prim-seq (make-parameter '()))
  
  (define gensym (make-gensym (prefix "r")))
  (define (make-node)
    (let ((node (gensym)))
      (push! nodes node)
      node))
  (define (make-typed-node _) (make-node))  ;; FIXME
  
  (define (make-named-nodes ns)
    ;; FIXME: save orig name in assoc list.  We can't risk
    ;; non-uniqueness.
    (for/list ((n ns)) (make-node)))

  (define (default sym fn)
    (lambda (sem . args)
      (let ((node (make-node)))
        (push! prim-seq (list node sym fn args))
        node)))

  (define (feedback/n sem state-names
                      in-nodes
                      time-names
                      update)
    (let*
        ((nb-state   (length state-names))
         (si-nodes   (make-named-nodes state-names))
         (time-nodes (make-named-nodes time-names))
         (all-in     (append si-nodes in-nodes time-nodes))
         (all-out    (values-list (apply (ai-function-proc update) sem all-in)))
         (so-nodes   (take all-out nb-state))
         (out        (drop all-out nb-state)))
      (nsave state-out so-nodes)
      (nsave state-in  si-nodes)
      (apply values out)))
  
  ;; (define (reduce/n sem T reduce init body)
  ;;   (let* ((i (make-typed-node Int))
  ;;          (n (make-typed-node Int)))
  ;;     (parameterize ((loop (cons i (loop)))
  ;;                    (prim-seq '()))
  ;;       (let* ((accu-inputs
  ;;               (values-list
  ;;                ((ai-function-proc body) sem i n)))
  ;;              (nb-accu-inputs (length accu-inputs))
  ;;              ;; A bit inelegant, but if the arity of the fold is not
  ;;              ;; fixed, it is assumed to be a 2->1 fold where each
  ;;              ;; input corresponds to an accumulator.  This is used
  ;;              ;; to implement `bus'.
  ;;              (reduce-args (ai-function-args reduce))
  ;;              (nb-accus (if (list? reduce-args)
  ;;                            (- (length reduce-args) nb-accu-inputs)
  ;;                            nb-accu-inputs))
  ;;              (accumulators (make-vars nb-accus))
  ;;              (accu-outputs
  ;;               (values-list
  ;;                (apply (ai-function-proc reduce) sem
  ;;                       (append accumulators
  ;;                               accu-inputs))))
  ;;              (init-exprs
  ;;               (values-list
  ;;                ((ai-function-proc init) sem nb-accus))))
  ;;
  ;; FIXME: it's simpler to stick to ai-array's first step.

  ;; What is needed is a good intermediate form for code.
        
      

  
  (define semantics
    (make-ai
     #:default    default
     #:feedback/n feedback/n
     ))

  ;; PASS 1: Create node structure + and serialzed primitive sequence.
  (define in-nodes  (make-named-nodes (ai-function-args program)))
  (define out-nodes (values-list (apply (ai-function-proc program) semantics in-nodes)))
  (set! prim-seq (reverse prim-seq))
  
  (pp state-in)
  (pp state-out)

  ;; PASS 2: Perform analysis.

  ;; PASS 3: Create replayer function.
  (make-ai-function
   (lambda (parent-semantics . args)
     (define bindings (make-hasheq))
     (define (ref nodes) (for/list ((n nodes)) (hash-ref bindings n)))
     (for ((i in-nodes)
           (a args))
       (dict-set! bindings i a))
     (for ((s state-in))
       (dict-set! bindings s 0)) ;; FIXME: ???
       
     (for ((p prim-seq))
       (let-values (((r sym fn as) (apply values p)))
         (pp (list r sym as))
         (dict-set!
          bindings r
          (apply (ai-function-proc fn)
                 parent-semantics
                 (ref as)))))
     (apply values (ref out-nodes)))
   (ai-function-args program)))

  
            
       
             
    
(begin-stream
 ;; (require "stream-lib.rkt")
 (define (test1 (s) (a b))
   (values s
           (/ (* a b) (+ s (- a b))))))

(require "ai-eval.rkt")

(define aprog (ai-annotated test1))
(define eprog ((ai-function-proc aprog) ai-eval-semantics
               100 1))

eprog
