#lang scheme/base
(provide (all-defined-out))
(require racket/string
         racket/match
         racket/system
         racket/list
         racket/dict)

;; Basic idea is to have a way to analyze circuits both for designing
;; actual analog, and for then approximating those and turning them
;; into C code.

;; As a driver ciruit, use a differential pair driven by a current
;; source, with NPNs expressed as an Ebers-Moll model.

;; As a way to input, use spice-like netlists.  Why not make it
;; compatible from the start, and make schematics with gschem?

(define (nb-terminals comp)
  (let ((dict '((T . 1)
                (Q . 3)
                (V . 2)
                (R . 2))))
    (dict-ref dict comp)))

(define (netlist file)
  (for/list
      ((line (in-lines (open-input-file file)))
       ;; Skip comments
       #:when (not (regexp-match #rx"^[\\*+.]" line))
       ;; Ignore anything past .end
       #:break (regexp-match #rx"^\\.end" line)
       )
    (match (string-split line)
      ((cons name args)
       (let*-values
           (((tag)      (string->symbol (substring name 0 1)))
            ((inst)     (string->symbol (format "_~a" (substring name 1))))
            ((nb)       (nb-terminals tag))
            ((nets val) (split-at args nb)))
         (list tag
               inst
               val
               (map string->number nets)
               ))))))

;; Build a graph representing the network.
;; Every net corresponds to a node with 2 or more edges

;; A Net has:
;; - a signgle V,dV/dt
;; - a I.dI/dt for each connected port (KCL: sum I = 0)
;; - a port is represented by (instance, index)
;; - an instance is (type name) iso (tag inst val)
;; - type = (tag val)
;; - name = (tag inst)

(define sem-graph
  `((T .,(lambda (g tiv n)
           ;; Terminals are abstracted as voltage sources.
           g))
    (Q .,(lambda (g tiv c b e)
           g))
    (V .,(lambda (g tiv p m)
           g))
    (R .,(lambda (g tiv a b)
           g))
    ))


;; An interesting analysis is to build a small-signal model.  Since
;; that will be a circuit consisting only of 2-terminals, it is a
;; proper non-simple graph and can be visualized with GraphViz.  A
;; 2-terminal can then be represented as edge -> box -> edge.  Nodes
;; can then be drawn smaller.



(define (test file)
  (system (format "make ~s" file))
  (netlist file))

(test "vca.spice")

