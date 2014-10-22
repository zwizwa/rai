#lang scheme/base
(require scheme/pretty
         scheme/dict)

;; Simple analog circuit simulator

;; - Basic arch: how to turn network into procedural code? I.e. not
;;   all components are "directional".  Most are relational.
;;   -> linearize all equations

;; - Opamp: probably won't work if not reactive.
;;   -> feedback needs decoupling

;; Diode model, linearized in parameters: all non-linearity is
;; "pipelined" into coefficients to allow linear system solving.

;;   Id/Is = e(Vx0/VT) (1 + [Vx-Vx0]/VT)

(define-syntax-rule (nport . fms) 'fms)

;; Equations in normalized form: (equations eq eq ...) where eq = 0

;; To make this do anything, start with something simple: an RC filter.

;; The basic building block is an n-port.
;; A complete circuit is a 0-port.

;; Connecting (equating) nodes means all v are the same and the sum of
;; currents is 0.

;; All nodes are explicit. It is up to the user to not connect them.
;; A non-connected node is a node connected to a 0 current source with
;; a floating, non-specified voltage.

(define p
  (nport
   ;; n-port composition is terminal node connection:
   ;; For each connected node, voltages are equal and currents sum to 0.
   ;; Current is flowing into node from the
   (terminals
    (v_in  i_in)
    (v_out i_out)
    (v_gnd i_gnd))
   ;; Intermediate variables used to factor equations.
   (intermediates
    (v_c))
   ;; Externally specified parameters, i.e. matrix coefficients.
   (parameters
    (R C))
   ;; System equations
   (equations
    (- v_c (- v_out v_gnd))
    (+ i_gnd (* C (d/dt v_c)))
    (- i_in  (* R (- v_in v_out)))
    ;; n-ports do not have hidden supply nodes, so this current sum
    ;; equation always holds for terminal nodes.  Does it need to be
    ;; explicit?
    (+ i_in i_out i_gnd)
    )))
 
   ;; Diode
   ;; (- (/ Id Is) (exp (/ Vx Vt)))

;; mapaccum?

(define (fold-subexpressions equation fn [s '()])
  (if (not (pair? equation)) s
      (for/fold
          ((s (fn equation s)))
          ((expr (cdr equation)))
        (pretty-print expr)
        (fold-subexpressions expr fn s))))

(define (fold-equations-subexpressions eqs fn [s '()])
  (for/fold
      ((s s))
      ((eq eqs))
    
    (fold-subexpressions eq fn s)))

(define (nport-equations p) (dict-ref p 'equations))

(define (collect fn)
  (lambda (expr s)
    (let ((v (fn expr)))
      (if v (cons v s) s))))
  
(define (derivatives p)
  (fold-equations-subexpressions
   (nport-equations p)
   (collect
    (lambda (expr)
      (and (pair? expr)
           (eq? 'd/dt (car expr))
           (cadr expr))))))
