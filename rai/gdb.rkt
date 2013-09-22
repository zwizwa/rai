#lang racket/base
(require racket/file
         racket/system
         racket/pretty
         racket/async-channel
         racket/match
         racket/dict
         "hex.rkt"
         "gdb-parse.rkt")

;; Experimenting with GDB's MI


(provide (all-defined-out))

(define gdb-instance (make-parameter #f))
(define-struct gdb (stdout stdin pid stderr control
                    thread-stdout thread-stderr channel) #:transparent)

(define (gdb-kill)
  (let ((g (gdb-instance)))
    ((gdb-control g) 'kill)
    (close-input-port  (gdb-stdout g))
    (close-input-port  (gdb-stderr g))
    (close-output-port (gdb-stdin  g))
    (thread-wait (gdb-thread-stdout g))
    (thread-wait (gdb-thread-stderr g)))
  (gdb-instance #f))

(define (gdb-start #:cmdline (cmdline "arm-eabi-gdb-7.6.1 -i=mi"))
  (unless (gdb-instance)
    (let* ((process-info (process cmdline))
           (stderr (list-ref process-info 3))
           (stdout (list-ref process-info 0))
           (channel (make-async-channel))
           (t-stderr (thread (lambda () (line-dispatch stderr stderr-handle channel))))
           (t-stdout (thread (lambda () (line-dispatch stdout mi-handle channel)))))
      (gdb-instance
       (apply make-gdb
              (append process-info
                      (list t-stderr t-stdout channel)))))))

;; Dispatcher task body.  Abort on error or EOF.
(define (line-dispatch port handle channel)
  (with-handlers
      ((void void))
    (let loop ()
      (let ((line (read-line port)))
        (unless (eof-object? line)
          (handle line channel)
          (loop))))))

;; Geiser gets confused when async i/o hits stdout, so log things to file instead.
(define (gdb-log . args)
  (with-output-to-file "/tmp/gdb-mi.log"
    (lambda () (apply printf args))
    #:exists 'append))

;; Does this actually get any data?
(define (stderr-handle line channel)
  (gdb-log "stderr: ~a\n" line))

;; Handle mi protocol
;; https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI-Output-Syntax.html#GDB_002fMI-Output-Syntax

(define (mi-log-data tag-char data channel)
  (let* ((result (gdb-read-expr-list
                  (open-input-string data)))
         (port (open-output-string))
         (tag (string->symbol (list->string (list tag-char))))
         (msg (cons tag result)))
    (begin
      (pretty-print msg port)
      (gdb-log (get-output-string port)))
    (async-channel-put channel msg)))

(define *data* #f)
(define (mi-handle line channel)
  (let ((tag  (string-ref line 0))
        (data (substring  line 1)))
    (case tag
      ((#\() (void)) ;; "(gdb)" = ready for next command
      ((#\~ #\@ #\&) (gdb-log (read (open-input-string data))))
      ((#\^ #\= #\* #\+) (mi-log-data tag data channel))
      (else (gdb-log "untagged: ~a\n" line)))))


(define (autoconnect)
  (unless (gdb-instance)
    (gdb-start)
    (connect)))

(define (gdb-reply)
   (async-channel-get (gdb-channel (gdb-instance))))
;; Execute GDB MI command
(define (mi> cmd . args)
  (with-handlers
      (((lambda (ex)
          (and (exn:fail:filesystem:errno? ex)
               (= 32 (exn:fail:filesystem:errno-errno ex)))) ;; broken pipe
        (lambda (ex)
          (pretty-print ex)
          (gdb-kill)
          (gdb-start))))
    (autoconnect)
    (let ((p (gdb-stdin (gdb-instance))))
      (display (apply format cmd args) p)
      (newline p)
      (flush-output p)

      ;; Wait for reply.  For each command, threre is at least one ^
      ;; reply to use for synchronization.  Not 100% sure if this is
      ;; correct, or what to do with the other tags..  Also it's not
      ;; sure if it makes sense to have a task+channel indirection
      ;; instead of just reading from gdb's stdout in the current
      ;; thread.
      (let loop ()
        (let ((reply (gdb-reply)))
          ;; (pretty-print reply)
          (match reply
            ((list-rest '^ msg)
             (cdr reply))
            (else
             (loop)))))
      )))

;; Execute regular GDB command
(define (gdb> cmd)
  (mi> "-interpreter-exec console ~s" cmd))

(define (disconnect)
  (mi> "-target-disconnect"))
(define (connect [port 3333])
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

(define (disassemble [at "$pc"] [nb-bytes 10])
  (for/list
      ((d (dict-ref
           (mi> "-data-disassemble -s ~a -e \"~a + ~s\" -- 0"
                at at nb-bytes)
           'asm_insns)))
    (list (dict-ref d 'address)
          (dict-ref d 'inst))))
    
(define (register-values)
  (mi> "-data-list-register-values d"))
(define (register-names)
 (mi> "-data-list-register-names"))

(define (var-assign var val)
  (mi> "-var-assign ~s ~s" var val))

(define (var-evaluate-expression expr)
  (read (open-input-string
         (unpack
          (mi> "-var-evaluate-expression -f d ~s" expr)
          '(value)))))

(define (create-register-vars)
  (for ((n (in-range 16)))
    (mi> "-var-create r~s * $r~s" n n)))

;; (mi> "-var-create r1 * $r1")



(define (write-memory addr bytes)
  (mi> "-data-write-memory-bytes ~s ~a" addr (hex bytes)))

;; ref a list of tags or unpack procedures
(define (unpack data ref)
  (if (null? ref) data
   (let ((tag (car ref))
         (ref (cdr ref)))
     (unpack
      (if (procedure? tag)
          (tag data)
          (dict-ref data tag))
      ref))))

(define (read-memory addr nb-bytes)
  (unhex
   (unpack
    (mi> "-data-read-memory-bytes ~s ~s" addr nb-bytes)
    `(memory ,car contents))))


