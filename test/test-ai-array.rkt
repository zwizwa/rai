#lang racket/base
(require rai/tools
         rai/ai-array
         rai/ai-array-c
         rai/ai-symbolic
         rai/stream-syntax
         rai/stream-lib
         rai/stream-meta
         "test-lang.rkt"
         "test_pd.rkt")


(module test-progs rai/stream
 (require rai/stream-lib
          rai/stream-meta)
 (provide (all-defined-out))
 (define (test-if< a b)  (if (< a b) b a))
 (define (test-if c a b) (if c a b))
 (define (test-< a b)    (< a b))
 )

(require 'test-progs)
  

(define (test program #:tc [tc '()] #:nsi [nsi 0])
  (display
   (ai-array-c
    program
    ;; twopole
    ;; integrate
    ;; nphasor
    ;; phasor
    #:tc tc
    #:nsi nsi
    )))

;; (test test-integrate-2D #:tc '((N . 3) (M . 4)) #:nsi 1)

(test test-poly)

(test test-t-exp-6)
(test test-t-sin-6)
(test test-t-cos-6)

  
(test test-pow-10)

(test test-inv)

(test test-dead-code)

;; (test test-dual-rate)
;; (test test-dual-rate2)



;; (test main)

(test test-hold)
(test test-setup)
(test test-setup-no-fb)
(test test-hold-inherit)

;; (test test-delay)

(test test-tag)

(display "**********************************\n")


;; (test test-<)

(test test-if)
(test test-if<)


;; Type error.
;; (test test-integrate-2D #:tc '((N . 3) (M . 4)) #:nsi 1)


;; (test nplus1)

