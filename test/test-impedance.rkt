#lang scheme/base
(require scheme/pretty)

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
;; The basic building block is an n-port.  A complete circuit is a 0-port.

(define p
  (nport
   (terminals
    ;; Current flowing into node from the outside.
    (v_in  i_in)
    (v_out i_out)
    (v_gnd i_gnd))
   (parameters
    (R C))
   (equations
    ;; n-ports do not have hidden supply nodes, so this always holds.
    ;; Does it need to be explicit?
    (+ i_gnd i_in i_out)
    
    (+ i_gnd (* C (d/dt (- v_out v_gnd))))
    (- i_in  (* R (- v_in v_out)))
    )))
 
   ;; Diode
   ;; (- (/ Id Is) (exp (/ Vx Vt)))
