#lang scheme/base
(provide hex unhex)
(define hexchar #"0123456789ABCDEF")
(define (hi-nibble b) (bitwise-and #xF (arithmetic-shift b -4)))
(define (lo-nibble b) (bitwise-and #xF b))
(define (hexbyte b)
  (list->bytes
   (list
    (bytes-ref hexchar (hi-nibble b))
    (bytes-ref hexchar (lo-nibble b)))))
(define (hex lst)
  (apply bytes-append (map hexbyte lst)))
(define (unhex hex)
  (for/list ((i (in-range 0 (string-length hex) 2)))
    (let ((hexbyte (format "#x~a" (substring hex i (+ 2 i)))))
      (read (open-input-string hexbyte)))))
