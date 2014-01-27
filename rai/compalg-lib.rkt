#lang racket/base
(require "stream-meta.rkt"
         "ai-symbolic.rkt"
         "ai-autodiff.rkt"
         "ai-eval.rkt"
         "ai-freq.rkt"
         "ai-stream.rkt"
         "stream-syntax.rkt"
         "tools.rkt"
         "gnuplot.rkt"
         plot)
(provide
 @ D
 _p _p1 _s _l _t

 ;; Plot scalar function over range
 plot-lin/log
 plot-lin/lin
 plot-log/log

 ;; Plot system over time
 plot-t

 ;; Frequency domain transfer function plot of a 1-1 linear system.
 plot-bode
 )

           

           

;; Some computer algebra tools based on the ai-symbolic interpretation.


;; The "prompt", which displays a program in symbolic form.

(define args-seq
  (sequence-append
   '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
   (sequence-map
    (lambda (n) (gensym (format "a~a" n)))
    (in-naturals))))

(define (symbolic-prog prog)
  (apply (ai-symbolic prog)
         (sequence-take args-seq
                        (length (ai-function-args prog)))))
(define @
  (make-ai-function
   (lambda (sem . progs)
     (for ((prog progs)) (pretty-print (symbolic-prog prog))))
   #f))

(define D
  (make-ai-function
   (lambda (sem prog [n 1])
     (unless (>= n 0)
       (error 'diff-negative))
     (for/fold
         ((prog prog))
         ((_ (in-range n)))
       (ai-deriv prog)))
   #f))


   

(define (ai-symbolic-printer fun port mode)
  (write-string (format "~a" (symbolic-prog fun)) port))

(ai-function-printer ai-symbolic-printer)

(define (ai-magnitude prog)
  (compose magnitude (ai-spectrum prog)))


;; GNUPLOT

(define _s
  (make-ai-function
   (lambda (_ prog)
     (let* ((spectrum (compose magnitude (ai-spectrum prog))))
       (plot-function spectrum
                      #:min -0.5
                      #:max +0.5
                      )))
   '(prog)))

(define _l
  (make-ai-function
   (lambda (_ . progs)
     (let* ((spectra (map ai-magnitude progs)))
       (plot-function-log/log-many
        spectra
        #:min 0.0001  ;; 4.8Hz
        #:max 0.5     ;; 24Hz
        )))
   '(prog)))
(define _p
  (make-ai-function
   (lambda (sem prog)
     (plot-function (ai-eval prog)))
   #'(prog)))
(define _p1
  (make-ai-function
   (lambda (sem prog)
     (plot-function (ai-eval prog)
                    #:min 0
                    #:max 1))
   #'(prog)))
(define _t
  (make-ai-function
   (lambda (sem n prog . args)
     (let ((n->f (sequence->function (apply (ai-stream prog) args))))
       (plot-function n->f
                      #:min 0
                      #:max n
                      #:points n)))
   #'(n prog . args)))





;; RACKET PLOT

(define (log/log thunk)
  (parameterize
      ((plot-x-transform log-transform)
       (plot-y-transform log-transform)
       (plot-x-ticks (log-ticks))
       (plot-y-ticks (log-ticks)))
    (thunk)))
  

(define plot-bode
  (make-ai-function
   (lambda (_  prog l r)
     (log/log
      (lambda ()
        (plot (function (ai-magnitude prog) l r)))))
   #'(prog)))
     

(define plot-lin/log
  (make-ai-function
   (lambda (_ prog l r)
     (parameterize
         ((plot-y-transform log-transform)
          (plot-y-ticks (log-ticks)))
       (plot (function (ai-eval prog) l r))))
   #'(prog)))

(define plot-log/log
  (make-ai-function
   (lambda (_ prog l r)
     (log/log
      (lambda ()
       (plot (function (ai-eval prog) l r)))))
   #'(prog)))


(define plot-lin/lin
  (make-ai-function
   (lambda (_ prog l r)
     (plot (function (ai-eval prog) l r)))
   #'(prog)))

(define plot-t
  (make-ai-function
   (lambda (sem n prog . args)
     (let* ((seq  (apply (ai-stream prog n) args))
            (table (for/list ((v seq)
                              (i (in-range n)))
                     (vector i v))))
       ;; (pp table)
       (plot (lines table))))
   #'(n prog . args)))
