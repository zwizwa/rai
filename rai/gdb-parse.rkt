#lang scheme/base

(provide gdb-read-expr
         gdb-read-expr-list)

;; Quick and dirty scheme-style descent parser using
;; `peek-string' and `read-string'



(define (read-tag)
  (let ((c (peek-char)))
    (when (or (eof-object? c)
              (memq c '(#\} #\] #\, #\=)))
      (unexpected c)))
  
  (let loop ((tag (list (read-char))))
    (let ((c (peek-char)))
      (if (or (eof-object? c)
              (memq c '(#\= #\{ #\} #\[ #\] #\,)))
          (string->symbol (list->string (reverse tag)))
          (begin (read-char)
                 (loop (cons c tag)))))))

(define (read-tag-val)
  (let* ((tag (read-tag))
         (val (if (eq? #\= (peek-char))
                  (begin (read-char) (read-expr))
                  '())))
    (cons tag val)))


(define (unexpected c)
  (error (format "unexpected: ~a" c)))


(define (read-list-balanced left right? read-element)
  (when left
    (let ((op (read-char)))
      (unless (eq? op left)
        (unexpected op))))
  (if (right? (peek-char))
      (begin
        (read-char) '())
      (let loop ((pairs (list (read-element))))
        (let ((c (peek-char)))
          (cond
           ((right? c)   (read-char) (reverse pairs))
           ((eq? c #\,)  (read-char) (loop (cons (read-element) pairs)))
           (else         (unexpected c)))))))

(define (eq?* c0) (lambda (c) (eq? c c0)))

(define (read-expr)
  (let ((c (peek-char)))
    (case c
      ((#\{) (read-list-balanced #\{ (eq?* #\}) read-tag-val))
      ((#\[) (read-list-balanced #\[ (eq?* #\]) read-expr))
      ((#\") (read)) ;; scheme-compatible string
      (else (read-tag-val)))))

(define (read-expr-list)
  (read-list-balanced #f eof-object? read-expr))


(define (reader fn)
  (lambda ([p #f])
    (if p
        (parameterize ((current-input-port p)) (fn))
        (fn))))

(define gdb-read-expr      (reader read-expr))
(define gdb-read-expr-list (reader read-expr-list))
              

;; ------------------------------------------------------------------
(define (test-parse str fn)
  (parameterize ((current-input-port (open-input-string str)))
    (fn)))

(define (test)
  (values
   (test-parse "abc=def"          read-tag-val)
   (test-parse "{abc=1,def=2}"    read-expr)
   (test-parse "[{abc=1,def=2}]"  read-expr)
   (test-parse "[{abc=1,def=\"a\tb\"}]" read-expr)
   (test-parse "abc,[{def=123}]"  read-expr-list)
   (test-parse
    "done,asm_insns=[{address=\"0x00000275\",inst=\"movs\\tr3, #0\"},{address=\"0x00000277\",inst=\"ldr\\tr2, [pc, #52]\\t; (0x2ac)\"},{address=\"0x00000279\",inst=\"ldr\\tr1, [pc, #52]\\t; (0x2b0)\"},{address=\"0x0000027b\",inst=\"adds\\tr0, r3, r2\"},{address=\"0x0000027d\",inst=\"cmp\\tr0, r1\"}]"
    read-expr-list)
   ))

;; (test)

