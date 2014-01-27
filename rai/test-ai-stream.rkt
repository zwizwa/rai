#lang racket/base
(require "ai-stream.rkt"
         "stream-lib.rkt"
         "test_pd.rkt"
         "tools.rkt"
         "test-lang.rkt")

;; Simple test macro.
(define-syntax-rule (t . a)
  (begin
    (display ";; ")
    (pretty-print '(t . a))
    (ai-stream . a)))

;; Transpose
(define (@ x)
  (apply values
         (transpose
          (sequence-take
           (sequence-map list x)
           20))))
;; Multi-valued
(define-syntax-rule (@s . e)
  (apply values (map @ (values-list . e))))

;; To identify dimensions:
;; 1 - number of inputs
;; 2 - number of output accumulated streams
;; 3 - number of elements inside accumulation loop
;; 4 - time values
;(@s ((t test-accu-dimensions-2-3)
;     '((1 2 3)
;       (4 5 6)
;       (7 8 9)
;       (10 11 12))))

(@ ((t integrate) '(1 1 1 1 1 1 1)))

(@ ((t integrate) '(1 2 3)))
(@ ((t z^-1)      '(1 2 3 4)))


(@ ((t phasor)    (make-list 10 .25) 0 1))

;; (values-list ((t test-vector) 1 2 3))


;; Pd test
;;((t main #:tc main-tc)
;; (in-range 0 1 .125) 1 1 .1)

(@ ((t test-svf) .1))




(@s ((t test-hold) '(1 2 3)))

(@ ((t test-setup) '(0 0 0 0 0 )))



(@ ((t test-setup-no-fb) '(1 2 3 4 5 6)))


(@s ((t test-hold-fb) '(1 2 3)))

(@ ((t test-time) '(0 0 0 0 0 0 0)))

(@ ((t test-time2) '(0 0 0 0 0 0 0)))


;; FIXME: check if this is correct (arrays of input streams?)
(@ ((t nphasor) (in-range 0 1 .125)))
(@ ((t test-integrate-2D) (in-range 0 1 .125)))



;; FIXME: tags only work when there are parameters.  ai-stream.rkt is
;; on top of ai-proc.rkt which uses all stream inputs.

;; (@ ((t test-tag) '( 0 0 0 )))

