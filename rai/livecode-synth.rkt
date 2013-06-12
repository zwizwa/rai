#lang racket/base
(require "livecode-ai-array-c.rkt"
         racket/system)

;; Experimentail live coding.  See Makefile
;; FIXME: hardcoded path

(define (compile-g.h code)
  (let ((file "synth.g.h"))
    (with-output-to-file file
      (lambda () (display code))
      #:exists 'replace)
    (eprintf "wrote ~a\n" file)
    (system "make synth.sp")))

(livecode-ai-array-c-poll-loop!
 (file "/home/tom/rai/rai/synth.rkt")
 compile-g.h)




