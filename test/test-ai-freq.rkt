#lang racket/base
(require
 ;; Compiler macro
 rai/tools
 rai/ai-freq
 rai/ai-linpar
 rai/ai-eval
 rai/stream-syntax
 rai/stream-lib
 "test-lang.rkt"
 "test_pd.rkt"
 )

(module test-progs rai/stream
 (require rai/stream-lib
          rai/stream-meta)
 (provide (all-defined-out))
 
 (define (test-if< x)
   (let* ((x (z^-1 x))
          (x (+ 1 x)))
     (if (< x (+ x 1))
         (* 2 x)
         (* 3 x))))

 (define (test-id x)
   x)
 )

(require 'test-progs)
 


;; (define L (make-ai-z (lambda (z) z)))

(define z.1 (make-polar 1 .1))


((ai-ztx z^-1) z.1)
((ai-spectrum z^-1) .1)

((ai-spectrum diff) .1)


(with-handlers
    ((exn? (lambda args (printf "~a\n" args))))
  ((ai-spectrum deriv-x2) .1))

((ai-spectrum deriv-x1) .1)

(define zz (ai-freq z^-1))
(define zzo ((ai-function-proc zz) ai-eval-semantics (make-ai-z 123 (lambda _ 1))))
zzo

((ai-spectrum test-svf) .1)

;;((ai-ztx test-id) z.1)

(display "***\n")
((ai-ztx test-if<) z.1)

;; (ai-freq z)


;; ((ai-z z) ai-eval-semantics L)


;(((ai-freq z) L) z.1)

;; (((ai-freq integrate) 1) z.1)


#;(

(((ai-freq onepole) 1 .123) z.1)
(((ai-freq test-z3) 1) z.1)

(define pi (* 4 (atan 1)))

(define (tx p f)
  (magnitude (((ai-freq p) 1)
              (make-polar 1 f))))

(for/list ((f (in-range 0 3.0 .05)))
  (tx test-avg f))

)
