#lang scheme/base

;; Monitor file, trigger recompile with pre-loaded compiler.
;; ex: racket fam-sp.rkt synth.rkt

(require (planet jao/mzfam:2:1/fam-task)
         "ai-proc.rkt"
         "synth-lib.rkt")

;; Anchor with most support modules pulled in.
(define-namespace-anchor anchor)

(define ft (fam-task-create))

;; Load module and instantiate it in a new namespace.
;; FIXME: This should probably reuse modules.
(define (instantiate file)
  (parameterize ((current-namespace
                  (namespace-anchor->empty-namespace anchor)))
    (namespace-require `(file ,file))
    (values (namespace-variable-value 'main)
            (namespace-variable-value 'main-nsi))))


(define (recompile .rkt)
  (let*-values
      (((_)        (printf "[.rkt] ~a\n" .rkt))
       ((proc nsi) (instantiate .rkt))
       ((_)        (printf "[proc]\n"))
       
       ;; FIXME: rules.mk requires .rkt in same dir as .g.h
       ((.g.h)     (regexp-replace ".rkt$" .rkt ".g.h"))
       ((_)        (printf "[.g.h] ~a\n" .g.h))
       ((.sp)      (ai-sp/.g.h proc .g.h nsi))
       ((_)        (printf "[.sp]  ~a\n" .sp)))
    ;; FIXME: notify Pd
    .sp))

(define (handle-event event)
  (let ((path (fam-event-path event))
        (type (string->symbol (fam-event-type->string (fam-event-type event)))))
    (case type
      ((Found Modified)
       (begin
         (recompile path))))))

(define (monitor file)
  (when (fam-task-add-path ft file handle-event)
    (fam-task-join ft)))

(monitor (vector-ref (current-command-line-arguments) 0))

