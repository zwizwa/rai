#lang scheme/base
(require scheme/file
         scheme/system
         scheme/pretty
         "gdb-parse.rkt")

;; Experimenting with GDB's MI

(provide (all-defined-out))

(define gdb-instance (make-parameter #f))
(define-struct gdb (stdout stdin pid stderr control thread-stdout thread-stderr) #:transparent)

(define (close-gdb)
  (let ((g (gdb-instance)))
    ((gdb-control g) 'kill)
    (close-input-port  (gdb-stdout g))
    (close-input-port  (gdb-stderr g))
    (close-output-port (gdb-stdin  g))
    (thread-wait (gdb-thread-stdout g))
    (thread-wait (gdb-thread-stderr g)))
  (gdb-instance #f))

(define (start-gdb #:cmdline (cmdline "arm-eabi-gdb-7.6.1 -i=mi"))
  (unless (gdb-instance)
    (let* ((process-info (process cmdline))
           (stderr (list-ref process-info 3))
           (stdout (list-ref process-info 0))
           (t-stderr (thread (lambda () (line-dispatch stderr stderr-handle))))
           (t-stdout (thread (lambda () (line-dispatch stdout mi-handle)))))
      (gdb-instance
       (apply make-gdb
              (append process-info
                      (list t-stderr t-stdout)))))))

;; Dispatcher task body.  Abort
(define (line-dispatch port handle)
  (with-handlers
      ((void void)) ;; just abort.  geiser doesn't like async printing..
    (let loop ()
      (let ((line (read-line port)))
        (unless (eof-object? line)
          (handle line)
          (loop))))))

;; Geiser gets confused when async i/o hits stdout, so log things to file instead.
(define (gdb-log . args)
  (with-output-to-file "/tmp/gdb-mi.log"
    (lambda () (apply printf args))
    #:exists 'append))

;; Does this actually get any data?
(define (stderr-handle line)
  (gdb-log "stderr: ~a\n" line))

;; Handle mi protocol
;; https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI-Output-Syntax.html#GDB_002fMI-Output-Syntax

(define (mi-log-data tag data)
  (let* ((result (gdb-read-expr-list
                  (open-input-string data)))
         (port (open-output-string)))
    (pretty-print (cons (string->symbol (list->string (list tag)))
                        result) port)
    (gdb-log (get-output-string port))))

(define *data* #f)
(define (mi-handle line)
  (let ((tag  (string-ref line 0))
        (data (substring  line 1)))
    (case tag
      ((#\() (void)) ;; "(gdb)" = ready for next command
      ((#\~ #\@ #\&) (gdb-log (read (open-input-string data))))
      ((#\^ #\= #\* #\+) (mi-log-data tag data))
      (else (gdb-log "untagged: ~a\n" line)))))

;; Execute GDB MI command
(define (mi> cmd . args)
  (with-handlers
      (((lambda (ex)
          (and (exn:fail:filesystem:errno? ex)
               (= 32 (exn:fail:filesystem:errno-errno ex)))) ;; broken pipe
        (lambda (ex)
          (pretty-print ex)
          (close-gdb)
          (start-gdb))))
    (let ((p (gdb-stdin (gdb-instance))))
      (display (apply format cmd args) p)
      (newline p)
      (flush-output p))))

;; Execute regular GDB command
(define (gdb> cmd)
  (mi> "-interpreter-exec console ~s" cmd))

(define (disconnect)
  (mi> "-target-disconnect"))
(define (connect [port 3333])
  (start-gdb) ;; start if necessary
  (disconnect)
  (mi> "-gdb-set target-async 1")
  (mi> "-target-select remote :~s" port)
  (gdb> "monitor reset init"))

(define (continue)  (mi> "-exec-continue"))
(define (interrupt) (mi> "-exec-interrupt"))

(define (reset-init)
  (gdb> "mon reset init")
  (gdb> "set $sp = *(void**)0")
  (gdb> "set $pc = *(void**)4"))

(define (dasm [n 5])
  (mi> (format "-data-disassemble -s $pc -e \"$pc + ~s\" -- 0"
               (* 2 n))))
       




;; (define parse-abort-fn (make-parameter (lambda () (raise 'parse-error))))
;; (define (parse-abort) ((parse-abort-fn)))

;; (define parse-seq (make-parameter #f))


;; (require scheme/sequence)
;; (define (port->stream p)   (sequence->stream (in-input-port-chars p)))
;; (define (string->stream s) (port->stream (open-input-string s)))

;; (define parse-in (make-parameter '()))
;; (define (parse-peek) (stream-car (parse-in)))


;; (define (tok str)
;;   (l
  
;;   (string->str
;;   (let loop ()
;;     (unless (eql
;;   (for ((t str) (i (parse-in)))
;;     (unless (equal? t i)
;;       (parse-abort))))
;; (define (upto chars)
;;   (let* ((char-list (

;; (define (test-parse str fn)
;;   (parameterize ((current-input-port (open-input-string str)))
;;     (fn)))

;; (require scheme/pregexp)
    

;(define (test)
;  (start-gdb)
;  (connect))


; (start-gdb)
; (connect)
; (dasm)


