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
       #:when (not (regexp-match #rx"^\\*" line))
       ;; Ignore anything past .end
       #:break (regexp-match #rx"^\\.end" line)
       )
    (match (string-split line)
      ((cons name args)
       (let*-values
           (((type)     (string->symbol (substring name 0 1)))
            ((name)     (string->symbol (format "_~a" (substring name 1))))
            ((n)        (nb-terminals type))
            ((nets rst) (split-at args n)))
         (list type
               name
               (map string->number nets)
               rst))))))

(define (test file)
  (system (format "make ~s" file))
  (netlist file))

(test "vca.sn")

