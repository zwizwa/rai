#lang scheme/base

;; Monitor file, trigger recompile with pre-loaded compiler.
;; ex: racket fam-sp.rkt synth.rkt

(require (planet jao/mzfam:2:1/fam-task)
         racket/system
         "ai-proc.rkt"
         "synth-lib.rkt")

;; Anchor with most support modules pulled in.
(define-namespace-anchor anchor)


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
       
       ;; rules.mk requires .rkt in same dir as .g.h
       ((.g.h)     (regexp-replace ".rkt$" .rkt ".g.h"))
       ((_)        (printf "[.g.h] ~a\n" .g.h))
       ((.sp)      (ai-sp/.g.h proc .g.h nsi))
       ((_)        (printf "[.sp]  ~a\n" .sp)))
    .sp))

(define (monitor file notify)
  (define ft (fam-task-create))
  (when (fam-task-add-path
         ft file
         (lambda (event)
           (let ((path (fam-event-path event))
                 (type (fam-event-type event)))
             (case type
               ((fam-event-found
                 fam-event-modified)
                (notify (recompile path)))))))
    (fam-task-join ft)))

(define (arg i)
  (vector-ref (current-command-line-arguments) i))

(if (= 2 (vector-length (current-command-line-arguments)))
    (monitor
     ;; File to watch.
     (arg 0)
     ;; Notify script
     (lambda (.sp)
       (system (format "~a ~a" (arg 1) .sp))))
    (printf "usage: racket fam-sp.rkt <file> <notify.sh>\n"))



