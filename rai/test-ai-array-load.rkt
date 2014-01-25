#lang racket

(require "tools.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         "stream-syntax.rkt"
         "stream-lib.rkt"
         "libproc.rkt")



(define .g.h (make-temporary-file "proc-~a.g.h" #f "."))
(define .sp  (regexp-replace ".g.h" (path->string .g.h) ".sp"))

(with-output-to-file .g.h
  (lambda ()
    (display (ai-array-c integrate)))
  #:exists 'truncate)

(system (format "make ~a" .sp))

(define sp (proc_load_sp .sp))
sp


