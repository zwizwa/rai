#lang racket/base
(require "tools.rkt"
         "stream-syntax.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         (for-syntax racket/base))


(module test "stream.rkt"
  (require "stream-lib.rkt"
           "stream-meta.rkt")
  (provide (all-defined-out))

  ;; Basic loop operation.
  (define (test-loop3 in)
    (loop (i (n 4))
          ((s 0))
          ((x in))
          (let ((sum (+ s (z^-1 x))))
            (values sum sum))))

  ;; Use of temporary array.
  (define (test-loop2 in)
    (let-values
        (((accu vec)
          (loop (i (n 4))
                ((s 0))
                ((x in))
                (let ((s (+ s (^ x 2))))
                  (values s s)))))
      (loop (i (n 4))
            ((s accu))
            ((x vec))
            (let ((s (+ s (^ x 2))))
              (values s s)))))

  ;; Multi-out test
  (define (test-mo x)
    (let ((_ (loop (i (n 4)) ((s 0)) ((x x)) s)))         
      x))
  
)
(require 'test)


(define (test program
              #:nsi [nsi 1])
  (display
   (ai-array-c
    program
    #:nsi nsi
    )))


(test test-loop3)
(test test-loop2)

;; (test test-mo)


