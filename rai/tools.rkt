#lang racket/base
(require racket/list
         racket/pretty
         racket/dict
         racket/promise
         racket/control
         racket/match
         racket/sequence
         racket/stream
         racket/promise
         racket/math
         syntax/id-table
         "number.rkt"
         )

(provide (all-defined-out)
         (all-from-out racket/list
                       racket/pretty
                       racket/dict
                       racket/promise
                       racket/control
                       racket/match
                       racket/sequence
                       racket/stream
                       racket/promise
                       racket/math
                       syntax/id-table
                       "number.rkt"
                       ))

(define-syntax-rule (values-list expr)
  (call-with-values (lambda () expr) list))

(define (transpose rows)
  (apply map (cons list rows)))

(define ((prefix str) x) (format "~a~a" str x))
(define (to-string fn)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (fn)
      (get-output-string port))))
  
(define ((syntax-prefix pfx) stx)
  (let* ((dat (syntax->datum stx))
         (str ((prefix pfx) dat))
         (sym (string->symbol str)))
    (datum->syntax stx sym)))

;; This is annoying.  The "spliced" structure of keywords makes it
;; hard to build abstractions on top of it.
;; ((k1 v1) (k2 v2) ...) -> (#:k1 (k1 v1) #:k2 (k2 v2) ...)
(define keyword
  (compose string->keyword symbol->string syntax->datum))
(define (keyword-list stx)
  (syntax-case stx ()
    (((v e) ...)
     (syntax-case (map/stx keyword #'(v ...)) ()
       ((k ...)
        (apply append
               (map syntax->list
                    (syntax->list #'((k (v e)) ...)))))))))

(define (syntax-append* syntaxes)
  (apply append (map syntax->list syntaxes)))

  

(define (map/stx fn . ls)
  (apply map fn (map syntax->list ls)))


(define (->datum x)  (syntax->datum #`#,x))
(define (->syntax x) (datum->syntax #f x))

;; I like mutation of accumulators stored in a lexical context.
;; Threading context through tree iterations is just too painful.
(define-syntax-rule (push! stack val)    (let ((v val)) (set! stack (cons v stack)) v))
(define-syntax-rule (npush! stack vals)  (for ((v vals)) (push! stack v)))
(define-syntax-rule (add! counter v)     (set! counter (+ counter v)))
(define-syntax-rule (pop! stack)
  (let ((v (car stack)))
    (set! stack (cdr stack)) v))

(define (false . _) #f)


;; Save to list wrapped in parameter or variable.
;; Support both lexical variables and dynamic parameters.
(define-syntax-rule (save lst val)
  (if (parameter? lst)
      (lst (cons val (lst)))
      (set! lst (cons val lst))))
(define-syntax-rule (nsave lst vals)
    (for ((v vals)) (save lst v)))
(define (collect lst)
  (reverse (if (parameter? lst) (lst) lst)))





;; Symbol generator
(define (make-counter [n 0])
  (lambda ()
    (let ((rv n))
      (set! n (add1 n))
      rv)))

(define (make-gensym [pfx (prefix "s")])
  (define count (make-counter 0))
  (lambda () (string->symbol (pfx (count)))))

(define (id=? a b)
  (and (identifier? a)
       (identifier? b)
       (bound-identifier=? a b)))

;; OCaml style type symbols: a .. z a1 .. z1 a2 ...
(define (ocaml-type-symbol n)
  (let* ((letter (remainder n 26))
         (suffix (quotient  n 26)))
    (string->symbol
     (format "~a~a"
             (integer->char
              (+ (char->integer #\a) letter))
             (if (zero? suffix) "" suffix)))))

;; Start at i
(define (index-symbol n)
  (ocaml-type-symbol (+ n 8)))

(define-syntax-rule (lambda* a . es)
  (lambda (lst) (apply (lambda a . es) lst)))


(define (make-element-of lst)
  (let ((h (make-hash
            (map (lambda (v) (cons (->datum v) #t)) lst))))
    (lambda (v)
      (dict-ref h v (lambda _ #f)))))


(define-syntax-rule (stderr . forms)
  (parameterize ((current-output-port (current-error-port)))
    (begin . forms)))
(define-syntax-rule (stderr-pp v)
  (stderr (pretty-print (->datum `(v ,v)))))
(define-syntax-rule (pp v)
  (stderr (pretty-print (->datum `(v ,v)))))

(define (c-list lst)
  (cond
   ((null? lst) "")
   ((null? (cdr lst)) (format "~a" (car lst)))
   ;; To make this work for macros containing symbols, don't add a
   ;; space before/after comma.
   (else (format "~a,~a" (car lst) (c-list (cdr lst))))))

(define (with-output-to-string thunk)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (thunk))
    (get-output-bytes port)))

(define (set-boxes! bs val)
  (if (list? val)
      (for ((b bs) (v val)) (set-box! b v))
      (for ((b bs)) (set-box! b val))))


(define (sequence-take seq n)
  (for/list ((s seq) (_ (in-range n))) s))
(define (sequence-car seq) (car (sequence-take seq 1)))
(define (sequence-cdr seq) (sequence-tail seq 1))

;; Transpose sequences: List of sequences -> sequence of lists.
(define (in-pseqs seqs)
  (in-values-sequence (apply in-parallel seqs)))

;; Sequence of lists -> list of sequences.
(define (transpose-sequence-of-list sol)
  ;; This is not defined to work on an empty sequence, since in that
  ;; case we do not know the arity.
  (for/list ((i (length (sequence-ref sol 0))))
    (sequence-map (lambda (lst) (list-ref lst i)) sol)))
  
  

(define (sequence->function seq)
  (let ((stream (sequence->stream (in-values-sequence seq))))
    (lambda (x)
      (let* ((n (floor/int x)))
        (unless (>= n 0)
          (error 'sequence->function:negative-index))
        (apply values (stream-ref stream n))))))

;; FIXME: use contract
(define (in-bits n [base 2])
  (when (< n 0)
    (error 'in-bits:negative (format "~a" n)))
  (if (zero? n) '()
      (let ((r (remainder n base))
            (q (quotient  n base)))
        (stream-cons r (in-bits q base)))))

;; Use lazy operations to avoid creating unused nodes for use in ai-array.rkt
(define (integer-power x n [* *] [one 1])
  (force
   (car
    (values-list
     (for/fold
         ((accu   (delay one))
          (square (delay x)))
         ((b (in-bits n)))
       (values
        (if (zero? b)
            accu
            (delay (* (force square)
                      (force accu))))
        (delay (* (force square)
                  (force square)))))))))

(define (floor/int x)
  (inexact->exact (floor x)))


(define (flatten-sequences x)
  (cond
   ((list? x)     (map flatten-sequences x))
   ((sequence? x) (flatten-sequences (sequence->list x)))
   (else
    (stderr-pp x)
    x)))
      
;; Find smallest power of two that will fit size.
(define (ceil-2^n x)
  (let try ((n 0))
    (let ((size (arithmetic-shift 1 n)))
      (if (<= x size)
          size
          (try (add1 n))))))

(define (not-in lst) (lambda (el) (not (memq el lst))))


;; Obtain smallest list length, #f if none is a list.
(define (min-length lists)
  (for/fold
      ((m #f))
      ((l lists))
    (if (list? l)
        (let ((len (length l)))
          (if m (min m len) len))
        m)))
        
            
            
      
             
