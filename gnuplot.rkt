#lang racket/base

(require
 "tools.rkt"
 racket/system
 racket/match)

(provide
 gnuplot
 open-gnuplot
 close-gnuplot
 current-gnuplot
 close-current-gnuplot
 ;; plot
 plot-many
 splot
 mplot
 lplot
 plot-function
 plot-function-log/log
 plot-function-log/log-many
 )

;; FIXME: Re-use plot server to allow racket restarts.

;; Together with spawn.c this gets rid of zombie processes. Child
;; processes get killed whenever their stdin is closed.

(define (open-output-process . cmdline)
  (define out (open-output-file "/tmp/output-process.log" #:exists 'append))
  (let-values
      (((proc
         stdout
         stdin
         stderr) (apply subprocess
                        out #f out
                        "/usr/local/bin/spawn"
                        cmdline)))
    (subprocess-wait proc)
    ;;;; Can't detect exec errors!
    ;; (unless (= 0 (subprocess-status proc))
    ;;  (error 'dfork-error))
    stdin))

(define current-gnuplot (make-parameter #f))

(define (close-current-gnuplot)
  (when (current-gnuplot)
    (close-gnuplot (current-gnuplot))
    (current-gnuplot #f)))

(define (need-current-gnuplot)
  (let ((p (current-gnuplot)))
    (unless p
      (current-gnuplot (open-gnuplot))))
  (current-gnuplot))
        
(define (open-gnuplot)
  (open-output-process "gnuplot"))

(define close-gnuplot close-output-port)

(define (gnuplot . args)
  (parameterize ((current-output-port
                  ;; (current-output-port)
                  (need-current-gnuplot)
                  ))
    (if (null? args)
        (flush-output)
        (apply printf args))))

;; (define (plot xs ys #:with [with 'lines])
;;   (gnuplot "plot '-' with ~s\n" with)
;;   (for ((x xs)
;;         (y ys))
;;     (gnuplot "~a ~a\n" x y))
;;   (gnuplot "e\n")
;;   (gnuplot))


(define (plot-many table #:with [with 'lines])
  (define table-width (length (car table)))
  (define (colspec n) (format "'~a' using 1:2 title '~a' with ~a"
                              (if (= n 1) "-" "")
                              n with))
  (gnuplot "plot ")
  (gnuplot (c-list (for/list ((n (in-range 1 table-width))) (colspec n))))
  (gnuplot "\n")
  ;; Each plot needs a separate x,y table.
  (for ((col (in-range 1 table-width)))
    (for ((row table))
      (gnuplot "~a ~a\n"
               (list-ref row 0)
               (list-ref row col)))
    (gnuplot "e\n"))
  (gnuplot))
  

(define (splot xs ys zs #:with [with 'lines])
  (gnuplot "splot '-' with ~s\n" with)
  (for ((x xs)
        (y ys)
        (z zs))
    (gnuplot "~a ~a ~a\n" x y z))
  (gnuplot "e\n")
  (gnuplot)
  (gnuplot))

(define (mplot rows
               #:plot (plot 'plot)
               #:with (with 'lines))
  (gnuplot "~a '-' matrix with ~a\n" plot with)
  (for ((row rows))
    (for ((el row))
       (gnuplot "~a " el))
    (gnuplot "\n"))
  (gnuplot "e\ne\n")
  (gnuplot))

(define (lplot row #:with (with 'lines))
  (mplot (list row) #:with with))
  

(define (plot-function f
                       #:min (min -1)
                       #:max (max +1)
                       #:points (n 1000))
  (define res (/ (- max min) n))
  (define xy
    (for/list ((i (in-range n)))
      (let ((x (+ 0.0 min (* i res))))
        (list x (f x)))))
  (plot-many xy)
  ;; (stderr-pp xy)
  ;; xy
  )

(define (gnuplot-png file)
  (gnuplot "set term pngcairo\n")
  (gnuplot "set output ~s\n" file)
  (printf "<~a>\n" file))

(define (plot-function-log/log f
                               #:min (min 0.001)
                               #:max (max 1.0)
                               #:points (n 2000))
  (plot-function-log/log-many
   (list f) #:min min #:max max #:points n))
                               
                               

(define (plot-function-log/log-many fs
                                    #:min (min 0.001)
                                    #:max (max 1.0)
                                    #:points (n 2000))
  (define log-res (/ (- (log max) (log min)) n))
  (define table
    (for/list ((i (in-range n)))
      (let* ((x (* min (exp (* i log-res))))
             (ys (for/list ((f fs)) (f x))))
        (cons x ys))))
  (gnuplot "set logscale xy 10\n")
  ;; (gnuplot-png "/tmp/plot.png")
  (plot-many table)
  ;; (stderr-pp xy)
  ;; xy
  )

                               

  
 

  
  

                       
                       