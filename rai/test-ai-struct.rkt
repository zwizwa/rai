#lang racket/base
(require "tools.rkt"
         "ai-array.rkt"
         "ai-array-c.rkt"
         "stream-syntax.rkt"
         "stream-lib.rkt"
         "test-lang.rkt")

(define (test p)
  (display (ai-array-c p
                       #:nsi 3
                       #:tc '((B . 3))
                       ))
  (pretty-print (->datum (ai-array p))))

;; (test
;;  (stream
;;   (lambda (xs)
;;     (let ((x (in xs)))
;;       (wrap x 0 1)))))


;; ;; Take a system `phasor' and lift it into a block processor.
;; (test
;;  (stream
;;   (lift phasor 1)))
         

;; (test
;;  (stream
;;   (lambda (freq max)
;;     (bus B  (bus B
;;          (values
;;           (integrate freq)
;;           (integrate (phasor freq (const B max)
;;                              1))))))))



;; (test
;;  (stream
;;   (lambda (f1 f2 f3 f4 f5 freq)
;;     (values
;;      (phasor f1 0 1)
;;      (fold 7 + 0 (i n)
;;      (fold 7 + 0 (i n)
;;      (fold 7 + 0 (i n)
;;            (phasor (* i freq) 0 1))))))))
                     

(test
 (stream
  (lambda (freq gate)
    (values
     (bus 2 ()
     (bus 2 ()
          (* gate (phasor freq 0 1))))))))
                     





;; (pretty-print
;;  (->datum
;;   (ai-scheme-stx
;;    (stream
;;     (lambda (freq)
;;       (fold 11 + 0
;;             (i n)
;;             (phasor freq 0 1)))))))