#lang scheme/base
(provide (all-defined-out))

;; Basic idea is to have a way to analyze circuits both for designing
;; actual analog, and for then approximating those and turning them
;; into C code.

;; As a driver ciruit, use a differential pair driven by a current
;; source, with NPNs expressed as an Ebers-Moll model.

;; As a way to input, use spice-like netlists.  Why not make it
;; compatible from the start, and make schematics with gschem?

(define (netlist file)
  (for/list
      ((l (in-lines (open-input-file file))))
    l))

             
      
      

