#lang racket/base
(require "tools.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         "ai-symbolic.rkt"
         "stream-syntax.rkt"
         "stream-lib.rkt"
         "stream-meta.rkt"
         "test_pd.rkt"
         "test-lang.rkt")

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


(display "**********************************\n")

(test test-tag)


;; Type error.
;; (test test-integrate-2D #:tc '((N . 3) (M . 4)) #:nsi 1)


;; (test nplus1)

