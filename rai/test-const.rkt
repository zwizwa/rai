#lang racket/base
(require "tools.rkt"
         "stream-syntax.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         (for-syntax racket/base))


(begin-stream
 (require "stream-lib.rkt")
 (define (test-const v c)
  (bus 3 ;; nb-voices
       ()
       (c)  ;; Constant over loop
       (+ v c)))
 (define (test-const-2 v c)
  (bus 3 ;; nb-voices
       ()
       (c)  ;; Constant over loop
       (bus 3 ;; nb-voices
            ()
            (c)  ;; Constant over loop
            (+ v c))))
 (define (test-const-3 v c)
  (bus 3 ;; nb-voices
       ()
       () 
       (bus 3 ;; nb-voices
            ()
            (c)  ;; Constant over loop
            (+ v c))))
 (define (test-const-4 v c)
  (bus 3 ;; nb-voices
       ()
       ()
       (let ((c1 (+ c 1)))
         (bus 3 ;; nb-voices
              ()
              (c1)  ;; Constant over loop
              (+ v c1)))))
 )


(define (test program
              #:tc  [tc '()]
              #:nsi [nsi 0])
  (display
   (ai-array-c
    program
    #:tc  tc
    #:nsi nsi
    )))

;(test test-const)
;(test test-const-2)
;(test test-const-3)
(test test-const-4)


