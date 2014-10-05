#lang racket/base
(require
 xml
 xml/path)

;; This is silly.  No standard way to map CDATA to string?
(define (extract-cdata data)
  (let* ([string (cdata-string data)]
         [matches (regexp-match #rx"<!\\[CDATA\\[(.*)\\]\\]>" string)])
    (list-ref matches 1)))


(define (mine f [tags '(code.srate)]) ;; code.init code.krate code.srate
  (define x (read-xml (open-input-file f)))
  (define s (xml->xexpr (document-element x)))
  (define l (se-path*/list tags s))
  (map extract-cdata l))

(define f "/home/tom/git/AxoStudio/objects/filter/lp1.axo")
(define (main [f f])
  (for ((i (mine f)))
    (printf "----\n")
    (display i)))

; (main)


     
