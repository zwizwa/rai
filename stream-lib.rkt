#lang s-exp "stream.rkt"
(require "stream-meta.rkt"
         (for-syntax scheme/base))
(provide (all-defined-out))

;; ** STREAM OPS **

;; Causal systems can be distinguished from pure functions as they
;; have an extra state input form.  They produce state update values
;; as the first couple of values in their output.

;; Standard engineering convention: the z in the z-transform
;; represents the prediction operator, z^-1 is the delay operator.
(define (z^-1 (s) (i)) (values i s))

(define (integrate (s) (i))
  (let* ((o (+ s i)))
    (values o o)))


;; Map phase value to the [0,1] representation interval.
;;
;; Note that floor(n) for n integer and n < 0 maps n -> n - 1.
;; This means that wrap01 for n integer and n < 0 maps n -> 1.
;;
;; This is OK for "phase" values, i.e. those that are at some point
;; inserted into cos(2*pi*phase) or sin(2*pi*phase), since all
;; integers map to a single point.

(define (wrap01 x) (- x (floor x))) 

;; [min,max] phase oscillator update with increment stream.
;; Most efficient when min = 0, max = 1.
(define (phasor (phase)
                (inc min max))
  (let* ((range (- max min))
         (inv_range (/ 1 range))
         (inc   (* inv_range inc))
         (_phase (wrap01 (+ phase inc))))
    (values _phase
            (+ min (* range phase)))))


;; 1st order discrete differentiator
(define (diff i)
  (- i (z^-1 i)))


;;; Discrete timers

(define (timer (count) (period))
  (let* ((not-expired (< count period)))
    (values
     (if not-expired (+ count 1) 0)
     (if not-expired 0           1))))

(define (gated-timer (count) (period gate))
  (let* ((expired (not (< count period))))
    (values
     (if gate
         (if expired
             0 (+ count 1))
         count)
     (float expired)
     ;; (if (and gate expired) 1 0)
     )))

(define (positive-edge (last) (input))
  (values input (< last input)))
(define (negative-edge (last) (input))
  (values input (< input last)))

(define (clocked-timer (count) (period clock))
  (gated-timer period (positive-edge clock)))
  


;; ** SCALAR OPS **

;; These still operate on streams, but they can be projected down to
;; scalars ops and lifted back to streams without loosing information.


(define (clipl x min)     (if (< x min) min x))
(define (clipu x max)     (if (< x max) x max))
(define (clip  x min max) (clipu (clipl x min) max))



;; Truncated Taylor series
(define (t-exp x n)
  (+ 1 (* x (t-exp-tail x n))))

;; The 'n' here refers to ???
(define (t-sin x n) 
  (* x (t-sin-tail (* x x) n)))
(define (t-cos-1 x n)
  (let ((x2 (* x x)))
    (* x2 (t-cos-tail x2 n))))
(define (t-cos x n)
  (+ 1 (t-cos-1 x n)))
  

(define (nats (n) ())
  (values (+ n 1) n))





;; ** SURFACE SYNTAX **

;; For binding, type and loop manipulation, Scheme surface syntax is required.

;; (define-syntax bus1
;;   (syntax-rules ()
;;     ((_ T l () e)       (bus1 T l e))
;;     ((_ T l (c . cs) e) (bus1 T l cs (let ((c (const T c))) e)))
;;     ((_ T (i) e)        (bus1 T (i n) e))
;;     ((_ T () e)         (bus1 T (i n) e))
;;     ((_ T (i n) e)      (reduce T reduce-+ init-0 (i n) e))
;;     ))

;; ;; Old syntax.  Default to array.
;; (define-syntax bus
;;   (syntax-rules ()
;;     ((_ (ctor . ca) . a) (bus1 (ctor . ca)  . a))
;;     ((_ size . a)        (bus1 (Array 'size) . a))  
;;     ))


;; ;; Use syntax-case: the ... pattern in the expansion doesn't seem to
;; ;; work with syntax-rules.
;; (define-syntax (bus__ stx)
;;   (syntax-case stx ()
;;     ((_ bus-size
;;         ()
;;         (c ...)
;;         body)
;;      #`(ai-for (Array 'bus-size)
;;                (lambda (s) ;; FIXME: use vector state for multiple summation
;;                  (+ s
;;                     (let ((c (const (Array 'bus-size) c)) ...)
;;                       body)))
;;                (0)
;;                ()))))



(define-syntax mix
  (syntax-rules ()
    ((_ (i ...) ;; same as `loop' index spec
        vs body)
     (loop (i ...)
           ((accu 0))
           vs
           (+ accu body)))
    ((_ size streams body)
     (mix size () streams body))))



;; Annotated parameters
(define-syntax params
  (syntax-rules ()
    ((_ sc () body)
     body)
    ((_ sc ((p crv nm unt mn mx) . ps) body)
     (let ((p (tag p `((scale . ,sc)
                       (curve .  crv)  ;; literal
                       (name  . ,nm)
                       (unit  . ,unt)
                       (min   . ,mn)
                       (max   . ,mx)))))
       (params sc ps body)))))


;; FIXME: Currently there's a problem with requiring only the base
;; type to be of a certain type.  E.g. it might be wrapped in a bunch
;; of constructors.  Currently, `cast' will just decouple the
;; unification completely.
(define-syntax-rule (::Float x) (cast 'ignored x))

(define (int x)   (cast Int x))
(define (float x) (cast Float x))


;; TO DECIDE:
;; There is a lot of useful closed code in the Kmook project.
;; Some of that might be opened up.
;; Drop me a line if you're interested.
;; - Tom
