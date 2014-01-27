#lang s-exp "stream.rkt"
(require "compalg-lib.rkt"
         "stream-lib.rkt"
         "synth-lib.rkt"
         "test-lang.rkt")

;; This file is an example of a computer algebra worksheet.  This is
;; essentially a target language module (see #lang above) with some
;; extra functions imported from the library compalg-lib.rkt

;; This library overrides `ai-function-printer', printing all
;; values (stream functions) in symbolic form.

;; Define a stream lang function.
(define (test x) (^ x 10))

;; This is useful for printing the result of program transformation
;; operators, such as derivatives.
(D test)
(D (D test))

;; To see graphical plots, run this file in drracket / geiser.

;; Scalar plots
(plot-lin/lin test 0 1)
(plot-log/log test 0.01 1)

;; Stream plots: time
(plot-t 500 saw-d1 .01)



;; FIXME: system functions with time/space folds do not have a symbolic printer
;; saw-d1
