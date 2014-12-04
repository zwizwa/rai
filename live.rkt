#lang scheme/base
;; (require (for-syntax scheme/base))

;; Collect all numbers
(define (nums stx)
  (define params '())
  (define (collect-number! f)
    (let* ((n (length params))
           (p (datum->syntax
               #f (string->symbol
                   (format "_p~s" n)))))
      (set! params (cons `(,p ,n) params))
      p))
  (define (traverse! stx)
    (let ((expr (syntax-e stx)))
      (if (list? expr)
          (map traverse! expr)
          (let ((datum (syntax->datum stx)))
            (if (number? datum)
                (collect-number! datum)
                stx)))))
  (syntax-case stx ()
    ((_ (a ...) f)
     (let ((form (traverse! #'f))
           (ps (reverse params)))
       #`(lambda (a ... #,@ps) #,form)))))
