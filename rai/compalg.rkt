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

;; A symbolic form printer is installed by compalg-lib.rkt
test

;; This is useful for printing the result of program transformation
;; operators, such as derivatives.
(D test)
(D (D test))

;; To see graphical plots, run this file in drracket / geiser.
(plot-lin/lin test 0 1)
