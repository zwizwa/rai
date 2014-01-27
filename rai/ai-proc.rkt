#lang racket/base
(require "tools.rkt"
         "ai-array-c.rkt"
         "libproc.rkt"
         racket/runtime-path
         racket/file
         racket/system)

(provide ai-proc)

;; Convert stream function to C code, compile and run.

(define-runtime-path rai-dir ".")
(define-runtime-path rules.mk "rules.mk")
(define build-dir (find-system-path 'temp-dir))


(define (ai-sp proc nsi)
  (let* ((.g.h (make-temporary-file "proc_~a.g.h" #f build-dir))
         (.sp (regexp-replace ".g.h$" (path->string .g.h) ".sp")))
    (with-output-to-file .g.h
      (lambda ()
        (display (ai-array-c proc #:nsi nsi)))
      #:exists 'truncate)
    (let ((compile-out
           (with-output-to-string
             (lambda ()
               (let ((cmd (format "make RAI=~a -f ~a -C ~a ~a"
                                  rai-dir rules.mk build-dir .sp)))
                 (printf "~a\n" cmd)
                 (system cmd))))))
      ;; (display compile-out)
      (delete-file .g.h)
      .sp)))

(define (ai-proc proc #:nsi [nsi #f])
  (let ((.sp (ai-sp proc nsi)))
    (let ((proc-class (proc-load-sp .sp)))
      (delete-file .sp)
      proc-class)))

  


