#lang racket/base
(require rai/ai-stream
         rai/stream-syntax
         rai/stream-lib
         rai/tools
         rai/test-tools
         "test_pd.rkt"
         "test-lang.rkt")


;; Some stream functions
(begin
  (module dsp rai/stream
    (require rai/synth-lib) ;; saw-d1
    (provide (all-defined-out))
    ;;(define (fmod1 x) (fmod x 1))
    )
  (require 'dsp))


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


(@ ((t wrap01)   '(-2 -1.9 -1 -0.9 0 0.9 1 1.9 2)))

;; Prims.  These come from stream-syntax.rkt
(@ ((t ai-floor) '(-2 -1.5 -1 -0.5 0 0.5 1 1.5 2)))


