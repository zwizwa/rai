#lang racket/base
;; Work with stream forms in a plain Racket module.
(require (for-syntax racket/base))
(provide (all-defined-out))


(define-syntax (begin-stream stx)
  (syntax-case stx ()
    ((_ . forms)
     #`(begin
         (module stream-forms "stream.rkt"
           (provide (all-defined-out))
           . forms)
         (require 'stream-forms)))))
         
             
