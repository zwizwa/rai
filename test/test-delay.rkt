#lang racket/base
(require rai/tools
         rai/stream-syntax
         rai/ai-array
         rai/ai-array-c
         (for-syntax racket/base))



(module test rai/stream
 (require rai/stream-lib
          rai/stream-meta)
 (provide (all-defined-out))
  
 (define (test-delay (dl) (x))
  (let ((sum (+ x 
                (dl-ref dl 1)
                (dl-ref dl 2)
                (dl-ref dl 3))))
    (values
     (dl-shift dl sum)
     sum)))

 ;; while dl-read is generic, dl-ref is shoe-horned into a
 ;; functional semantics.  the ai-array language only implements
 ;; in-place update.
 
 (define (test-delay2 (d1 d2) (x))
   (let ((sum (+ x 
                 (dl-ref d1 1)
                 (dl-ref d2 2)
                 (dl-ref d2 3))))
     (values
      (dl-shift d1 sum)
      (dl-shift d2 sum)
      sum)))

 (define (test-svec (s) (x))
   (vector 1 2 3))

 (define (var-delay (d0 line) (x d max))
  (let* ((d1 (hold (clip d 1 max)))
         (_  (hold (dl-ref line max)))  ;; dummy read sets max
         (x0 (dl-ref line d0))
         (x1 (dl-ref line d1))
         (mixed (* 1/2 (+ x0 x1))))
    (values d1
            (dl-shift line x)
            mixed)))

 (define (test-var-delay x d)
   (var-delay x d 1000))

 
 (define (foo x)
   (let ((fn (lambda (x) (+ x 1))))
     (fn x)))

 (define (bar x)
   (vector-sum
    (map (lambda (n) (+ x n))
         (vector 1 2 3 4 5 6))))

 (define (df (line) (in time))
   (let ((out (dl-ref line time)))
     (values (dl-shift line in)
             out)))

 (define (test-df (d) (x))
   (let ((d (map df d '(1 2 3))))
     (values d d)))
         

 (define (test-delay-map (lines) (x))
   (let-values
       (((acc lines)
         (loop (i (n 4))
               ((acc 0))
               ((line lines))
               (let* ((d-out (dl-ref line 100))
                      (d-in (+ x (* 1/2 d-out))))
                 (values
                  (+ acc d-in)
                  (dl-shift line d-in))))))
     (values lines acc)))
   
 
 )

(require 'test)

(define (test program
              #:tc  [tc '()]
              #:nsi [nsi 0])
  (display
   (ai-array-c
    program
    #:tc  tc
    #:nsi nsi
    )))

;; (test test-delay2)
;; (test test-svec)
;; (test test-var-delay)

;; (test bar)

;; (test test-df)


;; FIXME: delay and loop don't mix
;; (test test-delay-map)
