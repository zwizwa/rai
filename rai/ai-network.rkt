#lang racket/base

;; Evaluate as rsound network? / signal?

(require rsound  ;; Install via package manager
         "ai-proc.rkt"
         "libproc.rkt"
         ffi/vector
         "f32vector.rkt")

(provide ai-network)

;; rsound:
;;  - A signal? is a network witout inputs.
;;  - A network? is a function or a (in out maker) triplet.
;;    The maker returns a stateful procedure.


(define (ai-network f)
  (let* ((class (ai-proc f #:nsi #f))  ;; no param inputs
         (nin  (proc-class-nin  class))
         (nout (proc-class-nout class)))
    (network/s
     nin nout
     (lambda ()
       (let ((inst (proc-instantiate class))
             ;; Run at blocksize = 1
             (vin  (for/list ((i (in-range nin)))  (make/init-f32vector 1)))
             (vout (for/list ((i (in-range nout))) (make/init-f32vector 1))))
         (lambda args
           (for ((a args) (v vin)) (f32vector-set! v 0 (+ 0.0 a)))
           (proc-run! inst
                      (if (zero? nin) 1 vin)
                      vout) 
           (apply values
                  (for/list ((v vout))
                    (f32vector-ref v 0)))))))))

