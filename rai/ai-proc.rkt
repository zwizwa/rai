#lang racket/base
(require "tools.rkt"
         "ai-array-c.rkt"
         "libproc.rkt"
         racket/runtime-path
         racket/file
         racket/system)

(provide 
         ai-proc
         ai-proc-run-once
         ;; ai-sp ;; FIXME: should this be exported?
         )

;; Convert stream function to C code, compile and run
;; - Block parameters are not used: inputs are streams
;; - Only run once - don't keep track of state: allows to abstract away instantiation

(define-runtime-path build-dir ".")

(define (ai-sp proc)
  (let* ((.g.h (make-temporary-file "proc_~a.g.h" #f build-dir))
         (.sp  (regexp-replace ".g.h" (path->string .g.h) ".sp")))
    (with-output-to-file .g.h
      (lambda ()
        (display (ai-array-c proc)))
      #:exists 'truncate)
    (let ((compile-out
           (with-output-to-string
             (lambda () (system (format "make -C ~a ~a" build-dir .sp))))))
      ;; (display compile-out)
      (delete-file .g.h)
      .sp)))

(define (ai-proc proc)
  (let ((.sp (ai-sp proc)))
    (let ((proc-class (proc_load_sp .sp)))
      (delete-file .sp) 
      proc-class)))

;; Single run, don't keep state.
(define (ai-proc-run-once proc-class ins/n [outs #f])
  (let ((p (proc-instantiate proc-class)))
    (proc-run p ins/n outs)))
  


