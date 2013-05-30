#lang racket/base
(require
 "enter.rkt"
 "ai-array-c.rkt")

(provide
 livecode-ai-array-c-poll!
 livecode-ai-array-c-poll-loop!)

;; Live coding driver for continuous .rkt -> .g.h compilation.

;; Piggy-backs on modified "enter!" macro behaviour, since I don't
;; know what I'm doing.

(define need-compile (make-parameter #f))

(define (notify path)
  (need-compile #t)
  (eprintf "reload: ~a\n" path))

;; Returns #f or new program text.
(define-syntax-rule (livecode-ai-array-c-poll! module)
  (parameterize ((enter!-notify notify)
                 (need-compile #f))
    (enter! module)
    (and (need-compile)
         (compile))))
    
(define (compile)
  (ai-array-c (eval 'main)
              #:nsi (eval 'main-nsi)
              ))

(define-syntax-rule (livecode-ai-array-c-poll-loop! module compiler)
  (begin
    ;; Compile once before entering loop.
    (livecode-ai-array-c-poll! module)
    (compiler (compile))
    (let loop ()
      (let ((program (livecode-ai-array-c-poll! module)))
        (when program
          (compiler program))
        (sleep .1)
        (loop)))))
