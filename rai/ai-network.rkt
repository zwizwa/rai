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


(define (ai-network f [blocksize 1])
  (printf "blocksize ~a\n" blocksize)
  (let* ((class (ai-proc f #:nsi #f))  ;; no param inputs
         (nin  (proc-class-nin  class))
         (nout (proc-class-nout class)))
    (network/s
     nin nout
     (lambda ()
       (let ((instance (proc-instantiate class))
             (offset 0)
             (vin  (for/list ((i (in-range nin)))  (make/init-f32vector blocksize)))
             (vout (for/list ((i (in-range nout))) (make/init-f32vector blocksize))))
         (lambda args
           (for ((a args) (v vin))
             (f32vector-set! v offset (+ 0.0 a)))
           (set! offset (modulo (add1 offset) blocksize))
           (when (zero? offset)
             (proc-run! instance
                        (if (zero? nin) blocksize vin)
                        vout))
           (apply values
                  (for/list ((v vout))
                    (f32vector-ref v offset)))))))))

