#lang racket/base
(require "ai-proc.rkt"
         "tools.rkt"
         ffi/vector
         "f32vector.rkt")
         
(provide ai-stream)

;; Quick & dirty hack to implement the old ai-stream.rkt behavior on
;; top of ai-proc.rkt  (compile to C + load .sp object + run)

;; FIXME: Not all features work correctly.  There are a couple of
;; design inconsistencies.  To check:
;; - per block behavior (subsampling / control)
;; - arrays of input streams
;; - sequences are rendered to finite lists
;; - for all-scalar input, default size is fixed


;; Guess the block size from a mix of lists and scalars.
(define (guess-size ls default)
  (let ((n
    (for/fold
        ((n -1))
        ((l ls))
      (if (list? l)
          (max (length l) n)
          n))))
    (if (>= n 0) n default)))

(define (anylist->f32vector l)
  (list->f32vector (for/list ((e l)) (+ 0.0 e))))

(define ((->f32vector n) l)
  (cond
   ((list? l)     (anylist->f32vector l))
   ((number? l)   (make/init-f32vector n (+ 0.0 l)))
   ((sequence? l) (anylist->f32vector (sequence-take l n)))
   ))


(define (->f32vectors ls size)
  (map (->f32vector size) ls))




(define (ai-stream f [unwind-default 10])
  (let ((class (ai-proc f)))
    (lambda args
      (let* ((size (guess-size args unwind-default))
             (ins (->f32vectors args size)))
        (apply values
               (map f32vector->list
                    (ai-proc-run-once class ins)))))))

