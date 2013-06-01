#lang racket/base
(require "tools.rkt"
         "prim.rkt"
         "ai-eval.rkt"
         "ai-autodiff.rkt"
         "matrix-lib.rkt"
         "stream-syntax.rkt")
(provide ai-freq      ;; prog -> (inputs -> (z -> outputs))
         ai-spectrum  ;; prog, inputs -> (f -> complex_mag)
         (struct-out ai-z)
         ai-ztx
         )

;; Small-signal frequency domain analysis.

;; Basic strategy:
;; - normal eval: eval parent semantics lifted over small signals (linear approx)
;; - feedback: 
;;   - split variables in parameter / signal
;;   - compute output offsets
;;   - compute differential matrix of update function
;;   - return (memoized) z-dependent signals
;;     - compute z-dependent transfer function from z, matrix
;;     - apply transfer function



;; FIXME: Typical use is to compute the transfer function once and
;; then evaluate it for different frequencies.  Currently, a matrix
;; inversion is performed at each frequency.  It will be cheaper to
;; compute the transfer function as a rational function coefficients.

(define-struct ai-z (offset signal) #:transparent)

;; Fixed phase input signal.
(define (ai-freq-make-signal
         #:offset    [offset 0]
         #:amplitude [amplitude 1]
         #:lit       [lit (lambda (x) x)])
  (make-ai-z (lit offset)
             (lambda (z)
               (lit amplitude))))

(define (offset x)
  (if (ai-z? x) (ai-z-offset x) x))
(define (!z x)
  (not (ai-z? x)))


(define (ai-freq program)
        

  (define (freq-proc parent-semantics . inputs)

    (define (warn-small-sig tag)
      (stderr (printf "WARNING: ai-freq: small signal approx for `~a'\n" tag))
      ;; Only one warning per eval.
      (set! warn-small-sig void)
      )

    (define (f_op fun)
      (lambda args
        (apply fun parent-semantics args)))
    (define (p_op op)
      (f_op (ai-function-proc op)))

    ;; Delegate primitive operations.
    (define ^     (p_op ai-pow))
    (define +     (p_op ai-add))
    (define -     (p_op ai-sub))
    (define *     (p_op ai-mul))
    (define /     (p_op ai-div))
    (define iflt  (p_op ai-iflt))
    (define lit   (p_op ai-literal))
    
    ;; Delegate matrix operations.
    ;; FIXME: These should be meta operations instead of scheme functions.
    (define m_+   (f_op ai-matrix-add))
    (define m_-   (f_op ai-matrix-sub))
    (define m_*   (f_op ai-matrix-mul))
    (define v_*   (f_op ai-matrix-vector-mul))
    (define m_inv (f_op ai-matrix-inverse))
    
    (define (feedback/n freq-semantics
                        state-names
                        in-nodes
                        time-names
                        update
                        )
      
      ;; Time-variant stuff is not supported.
      (unless (zero? (length time-names))
        (error 'ai-freq:feedback/n:time-names
               (format "~a" time-names)))
      
      ;; Obtain the frequency response of a linear system by first
      ;; probing the function for the linear system matrix, and then
      ;; compute the effect of feedback through matrix inversion
      ;; (solve linear system).

      (let*
          ;; Run update once to determine node types.
          ((nb-state  (length state-names))
           (si-test   (append (make-list nb-state (ai-freq-make-signal)) in-nodes))
           (so-test   (values-list
                       (apply (ai-function-proc update)
                              freq-semantics
                              si-test)))
           
           ;; Create boxes containing #t for signal node and #f for
           ;; param node, and sort them into different categories for
           ;; later access.
           
           (node->box (compose box ai-z?))
           (si-bs     (map node->box si-test))
           (so-bs     (map node->box so-test))
           
           (si-L-bs   (filter unbox si-bs))      ;; linear state and input
           (i-L-bs    (drop si-L-bs nb-state))   ;; linear input (no state)
           (i-bs      (drop si-bs   nb-state))   ;; all input (no state)

           (so-L-bs   (filter unbox so-bs))      ;; linear state and output
           (o-L-bs    (drop so-L-bs nb-state))   ;; linear output (no state)
           (o-bs      (drop so-bs   nb-state))   ;; all output (no state)


           ;; Initialize from input nodes.  P nodes are not changed
           ;; below. L nodes will be overwritten in the probe loop.
           (_ (set-boxes! i-bs in-nodes))

           ;; Get offsets of linear nodes.
           (_ (set-boxes! so-bs so-test))
           (o-L-offsets (for/list ((b o-L-bs)) (ai-z-offset (unbox b))))

           ;; Save the z->amplitude functions for use in transfer
           ;; function evaluation.
           (i-L-z->amp (map unbox i-L-bs))
           (i-L-z (lambda (z) (for/list ((f i-L-z->amp)) ((ai-z-signal f) z))))

           ;; Gather matrix by computing partial derivatives of the
           ;; update function at offset points.

           ;; Lift program over the dual numbers to compute affine
           ;; approximation.  This is evaluated with linear inputs set
           ;; to dual numbers (offset + 1e) for the variable to which
           ;; we're deriving and (offset + 0e) for the constant
           ;; variables.
           (dual-update (ai-dual update))
           (lift-dual-in
            (lambda (deriv-box)
              (lambda (var-box)
                (make-dual
                 (offset (unbox var-box))
                 (lit (if (eq? var-box deriv-box) 1 0))))))


           (_ (set-boxes! si-bs si-test))
           (ABCD-matrix
            (transpose
             (for/list ((v si-L-bs))
               ;; Compute so = D_v(update)(si)
               (map set-box!
                    so-bs
                    (values-list
                     (apply (ai-function-proc dual-update)
                            parent-semantics
                            (map (lift-dual-in v) si-bs))))
               ;; Gather the linear nodes
               (map (compose dual-dx unbox) so-L-bs))))
           
           ;; (_ (stderr-pp ABCD-matrix))
                 
            
                          
           

           ;; Result is a linear state space equation representing
           ;; the s/i -> s/o function:
           ;;
           ;;   zs = A s + B i
           ;;    o = C s + D i
           ;;
           (m
            (let ((m (matrix-split nb-state nb-state ABCD-matrix)))
              (lambda (row col) (matrix-ref m row col))))
           
           (A  (m 0 0))
           (B  (m 0 1))
           (C  (m 1 0))
           (D  (m 1 1))


           ;; Eliminating s in the state space equation gives the
           ;; transfer function:
           ;;
           ;;   o = (C (zI - A)^1 B + D) i
           ;;
           (transfer-function
            (lambda (z)
              (let ((z^-1 (matrix-eye nb-state (^ z (lit -1)))))
                (m_+ (m_* C (m_* (m_inv (m_- z^-1 A)) B)) D))))

           ;; Add memoization based on the value of z, since nodes are
           ;; accessed independently but will most likely be accessed
           ;; with the same z.
           (z-last #f)
           
           (update-o-L!
            (lambda (z)
              ;; (stderr-pp z)
              ;; Compute z-dependence for linear inputs and transfer
              ;; function, and evaluate to obtain linear outputs.
              (let* ((i-L (i-L-z z))
                     (tx  (transfer-function z))
                     (o-L (v_* tx i-L)))

                ;; (stderr-pp `(,o-L ,tx ,i-L))

                ;; Eval transfer function for linear outputs.  The
                ;; values will be picked up by the memoized output
                ;; z-dependent functions.
                (set-boxes! o-L-bs o-L)
                (set! z-last z))))
              
           (make-memo-out (lambda (o-L-offset o-L-box)
                            (define (z->amp z)
                              (unless (eq? z z-last) (update-o-L! z))
                              (unbox o-L-box))
                            (make-ai-z o-L-offset z->amp))))
        ;; Return the memoized functions as linear outputs, next to
        ;; the untouched (numeric) parameter outputs.
        (set-boxes! o-L-bs (map make-memo-out o-L-offsets o-L-bs))
        (apply values (map unbox o-bs))))


    ;; Lift operations over offset + signal.  Note that it is
    ;; important to maintain the difference between ai-z and ordinary
    ;; parameters, as the distinction is made in feedback/n to compute
    ;; the feedback matrix.

    (define (nonlinear tag args)
      (error 'ai-freq:nonlinear (format "~a ~a" tag args)))

    (define (f-add/sub op)
      (match-lambda*
       ((list _ (struct ai-z (a0 a)) (struct ai-z (b0 b)))
        (make-ai-z (op a0 b0) (lambda (z) (op (a z) (b z)))))
       ((list _ (? !z a) (? !z b))
        (op a b))
       (args
        (nonlinear 'add/sub args))))
    (define f-add (f-add/sub +))
    (define f-sub (f-add/sub -))
    
    (define f-mul
      (match-lambda*
       ((list _ (struct ai-z (a0 a)) (? !z b))
        (make-ai-z (* a0 b) (lambda (z) (* (a z) b))))
       ((list _ (? !z a) (struct ai-z (b0 b)))
        (make-ai-z (* a b0) (lambda (z) (* a (b z)))))
       ((list _ (? !z a) (? !z b))
        (* a b))
       ((list _ (struct ai-z (a0 a)) (struct ai-z (b0 b)))
        ;; Small-signal approximation: drop modulation terms.
        (* a0 b0))))

    (define f-div
      (match-lambda*
       ((list _ (struct ai-z (a0 a)) (? !z b))
        (make-ai-z (/ a0 b) (lambda (z) (/ (a z) b))))
       ((list _ (? !z a) (? !z b))
        (/ a b))
       ;; FIXME: Small-signal approximation
       (args
        (nonlinear 'div args))))

    (define f-pow
      (match-lambda*
       ((list _ a n)
        (cond
         ((not (number? n))
          (error 'ai-freq:pow (format "~a" n)))
         ((= n (lit 0)) (lit 1))
         ((= n (lit 1)) a)
         ((and (integer? n) (> n 0)) 0) ;; small signal
         (else
          ;; FIXME: Small-signal approximation
          (nonlinear 'pow `(,a ,n)))))))
    
    (define f-iflt
      (match-lambda*
       ;; 2 signals -> map to 1 signal and 0.
       ((list sem (? ai-z? a) (? ai-z? b) y n)
        (f-iflt sem (f-sub sem a b) (lit 0) y n))
       ;; 2 parameters -> just compute
       ((list sem (? !z a) (? !z b) y n)
        (iflt a b y n))
       
       ;; Small signal approximation: set the signal amplitude to 0
       ;; for signals occuring in the comparison.  Because of previous
       ;; clause we know that either a or b is a signal.
       ((list _ a b y n)
        (iflt (offset a) (offset b)
              ;; params or ai-z parent just passes value
              y n))))
    
       
    ;; Feedback is special, all the rest inherits from other semantics.
    (define freq-semantics
      (make-ai #:feedback/n feedback/n
               #:literal    lit ;; CHECK THIS
               #:add        f-add
               #:sub        f-sub
               #:mul        f-mul
               #:div        f-div
               #:iflt       f-iflt
               #:pow        f-pow
               #:default    (lambda (sym fn)
                              (lambda args
                                (nonlinear sym args)))
               ))

    (apply (ai-function-proc program)
           freq-semantics
           inputs))
  
  (make-ai-function
   freq-proc
   (ai-function-args program)))
    

;; FIXME: for convenience.  ai-freq is fairly generic.  This routine
;; will reduce a proc to a concrete z->amp function based on
;; ai-eval-semantics, using only a single input.

      
(define (ai-ztx proc [in-sigs
                      (map (lambda _ (ai-freq-make-signal))
                           (ai-function-args proc))])
  (let* ((freq (ai-freq proc))
         (proc (ai-function-proc freq))
         (out-sigs (values-list (apply proc ai-eval-semantics in-sigs)))
         (z->amps (map ai-z-signal out-sigs)))
    ;; (stderr (printf "ai-ztx: offsets: ~a\n" (map ai-z-offset out-sigs)))
    (apply values z->amps)))


;; Compute the output spectrum of a signal as a map:
;; f \in [.5, .5] -> complex_mag

(define (ai-spectrum proc)
  (let ((ztx (ai-ztx proc)))
    (lambda (f)
      (let* ((z (exp (* +2i pi f)))
             (a (ztx z)))
        a))))
    
