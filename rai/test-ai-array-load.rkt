#lang racket

(require "tools.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         "stream-syntax.rkt"
         "stream-lib.rkt"
         "libproc.rkt"
         ffi/vector)

;; This is a simple form, not using block rate parameters.  All inputs
;; are stream inputs.

(define (ai-proc proc)
  (let*
      ((.g.h (make-temporary-file "proc-~a.g.h" #f "."))
       (.sp  (regexp-replace ".g.h" (path->string .g.h) ".sp")))
    (with-output-to-file .g.h
      (lambda ()
        (display (ai-array-c proc)))
      #:exists 'truncate)
    (system (format "make ~a" .sp))
    (let ((sp (proc_load_sp .sp)))
      (delete-file .g.h)
      (delete-file .sp)
      sp)))

;; Single run, don't keep state.
(define (proc-once proc-class ins/n [outs #f])
  (let ((p (proc-instantiate proc-class)))
    (proc-run p ins/n outs)))
  
(define sp-class (ai-proc integrate))

(define ins  (list (list->f32vector (map exact->inexact '(1 1 1 1 1 1)))))
(define outs (map f32vector->list (proc-once sp-class ins)))



