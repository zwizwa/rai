#lang racket/base
(require "tools.rkt"
         "prim.rkt"
         "type.rkt" ;; type-vector-size
         "stream-syntax.rkt")
(provide ai-stream)


;; High level reference implementation for Stream language.
;; Represented as operations on Racket sequences returning (lazy)
;; sequences.



;; Stream values are allowed to be lazy.
(define (scalar? x)
  (or (number? x)
      (box? x)       ;; state delay box
      (promise? x))) ;; see t_endx
(define (scalar-force x)
  (cond
   ((promise? x) (force x))
   ((box? x) (unbox x))
   (else x)))

(define (dont-force x) x)
(define (lift-to-sequence x [n #f])
  (if (scalar? x)
      (if n (make-list n x)
          (in-cycle (list x)))
      x))

;; Automatically lift scalars to streams, recursively, until all
;; arguments are scalars, then force scalar values.
(define (stream-apply fn args #:force (force dont-force))
  (let ((all-scalar (andmap scalar? args)))
    (if all-scalar
        (let ((forced-args (map force args)))
          ;; (stderr-pp `(,args ,forced-args ,force))
          (apply fn forced-args))
        (sequence-map
         (lambda (args) (stream-apply fn args #:force force))
         (in-pseqs (map lift-to-sequence args))))))

(define (ai-stream program
                   #:tc    [tc '()]
                   #:time  [time-seq (in-naturals)])

  (define (feedback/n sem
                      state-name/init
                      ins
                      time-names
                      update)

    (define in-seqs (map lift-to-sequence ins))
    (define nb-state (length state-name/init))

    ;; Lazy eval for time length.  This allows it to work for finite
    ;; sequences that reference T, and for infinite sequences that
    ;; don't.
    ;; FIXME: This doesn't work 100%: promises show up in the output.
    (define t_endx
      (delay (length (sequence->list (in-pseqs in-seqs)))))
            
    (define time-seqs
      (if (null? time-names)
          '()
          (list time-seq
                (lift-to-sequence t_endx))))
    
    ;; Constructing the state sequence needs needs some kind of lazy
    ;; knot-tying, which in Scheme can be done manually by using
    ;; boxes and some careful manipulation of the order of
    ;; evaluation.delay
    (define state-in-box-streams
      (for/list ((s0 (map cdr state-name/init)))
        (stream-cons (box s0)
                     (let tail ()
                       (stream-cons
                        (box #f)
                        (tail))))))

    ;; Output state boxes are a shifted version of the input state boxes.
    (define state-out-box-streams
      (map stream-rest state-in-box-streams))
    
    ;; Scalar values are automatically unboxed, so we just feed the
    ;; box stream state into the update procedure, wich will be lifted
    ;; over the streams and produce state and output (value) streams.
    ;; We just need to make sure the boxes get filled before they are
    ;; used by the primitives in the `update' method.
    (define state-out/out-seqs
      (values-list
       (apply (ai-function-proc update) sem
              (append state-in-box-streams in-seqs time-seqs))))
    
    (define state-out-val-seqs (take state-out/out-seqs nb-state))
    (define out-seqs           (drop state-out/out-seqs nb-state))
    
    ;; Implement the feedback delay as a side effect to forcing the
    ;; output stream.  I.e. fill the boxes in time.
    (define out-seqs/patch-delay
      (sequence-map
       (lambda args0
         (let* ((bs    (take args0 nb-state))
                (args1 (drop args0 nb-state))
                (vs    (take args1 nb-state))
                (os    (drop args1 nb-state)))
           ;; Make sure next state is patched before output is
           ;; returned.  As long as stream head gets forced before the
           ;; tail, things are OK.
           (for ((b bs) (v vs)) (set-box! b v))

           ;; If there is a straight path from state input -> output,
           ;; boxed values which are normally forced on-demand by the
           ;; primitives will appear in the output, so we force them
           ;; here.
           (map scalar-force os)
           ))
       (apply in-parallel
              (append state-out-box-streams
                      state-out-val-seqs
                      out-seqs))))
     (apply values
           (transpose-sequence-of-list
            out-seqs/patch-delay)))
            

  (define (for/n sem T body accu-inits ins/a index-names)
    ;; FIXME
    (apply values accu-inits))

  ;; (define (reduce/n sem ctor reduce init body)
  ;;   (define n (type-vector-size (ctor #f) tc))

  ;;   ;; The parallel part is automatically lifted.  It will produce a
  ;;   ;; structure with type nesting
  ;;   ;; (list_a   ;; from `values-list' : accumulation input arity
  ;;   ;;  (seq_t   ;; sequence ranging over time
  ;;   ;;   (seq_n  ;; sequence ranging over the accumulation loop values

  ;;   (define par-a-t-n
  ;;     (values-list 
  ;;      ((ai-function-proc body) sem (in-range n) n)))

  ;;   ;; To perform the fold, this needs to be transposed.  First bring
  ;;   ;; the time index to the top.
  ;;   (define par-t-a-n (in-pseqs par-a-t-n))

  ;;   ;; Then perform the inner transposition in a sequence-map, and do
  ;;   ;; the accumulation fold with the transposed result.
  ;;   (define out-t
  ;;     (sequence-map
  ;;      (lambda (seq-a-n)
  ;;        ;; (stderr-pp seq-a-n)
  ;;        (let*
  ;;            ((list-a-n
  ;;              (for/list ((x seq-a-n))
  ;;                (sequence->list
  ;;                 ;; Don't lift to infinite sequence.
  ;;                 (lift-to-sequence x n))))
  ;;             (list-n-a (transpose list-a-n))
  ;;             ;; FIXME: Obtain the arity of accumulators from the fold
  ;;             ;; routine arity.  This doesn't work for reduce-+ !
  ;;             (nb-accu (length list-a-n))
  ;;             (accu-fold-out
  ;;              (for/fold
  ;;                  ((accus 
  ;;                    (values-list
  ;;                     ((ai-function-proc init) sem nb-accu))))
  ;;                  ((accu-ins list-n-a))
  ;;                (values-list
  ;;                 (apply (ai-function-proc reduce) sem
  ;;                        (append accus accu-ins))))))
  ;;          accu-fold-out))
  ;;      par-t-a-n))

  ;;   ;; We now have (seq (list ...)), which will need to be transposed
  ;;   ;; to (values (seq ...))
  ;;   (apply values (transpose-sequence-of-list out-t)))
  
  (define (literal sem x) x)
  (define (debug sem x)
    (pp x)
    x)

  (define (hold/n sem exp)
    (define seqs (values-list ((ai-function-proc exp) sem)))
    (apply values (map (compose lift-to-sequence sequence-car) seqs)))

  (define (setup/n sem e0 e1)
    (let* ((seqs0 (values-list ((ai-function-proc e0) sem)))
           (seqs1 (values-list ((ai-function-proc e1) sem)))
           (heads (map (compose list sequence-car lift-to-sequence) seqs0))
           (tails (map (compose      sequence-cdr lift-to-sequence) seqs1))
           (outs  (map sequence-append heads tails)))
      (apply values outs)))
  
  ;; Primitives that perform calculations need to force the value.
  (define (prim binop #:force (force scalar-force))
    (lambda (sem . as) (stream-apply binop as #:force force)))
      
  (define semantics
    (make-ai #:feedback/n feedback/n
             #:for/n      for/n
             #:debug      debug
             #:literal    literal
             #:hold/n     hold/n
             #:setup/n    setup/n
             
             #:copy       (prim p_copy #:force dont-force)
             
             #:add        (prim p_add)
             #:sub        (prim p_sub)
             #:mul        (prim p_mul)
             #:div        (prim p_div)
             #:pow        (prim p_pow)

             #:floor      (prim p_floor)
             #:if         (prim p_if)
             #:lt         (prim p_lt)
             ))
  (lambda in
    (apply (ai-function-proc program)
           semantics in)))

