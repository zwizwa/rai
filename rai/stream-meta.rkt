#lang racket/base
(require "stream-syntax.rkt"
         "tools.rkt")
(provide
 ai-lambda-meta
 unpack
 unvector
 vector-sum
 ai-map
 ai-length
 ai-sum ai-prod
 poly tailor-approx
 t-exp-tail
 t-sin-tail
 t-cos-tail
 iterate
 reduce-+ init-0
 $
 )

;; The stream object language is first order to keep it "algebraic".
;; However, the meta language can be higher order: it is just Scheme.

;; The idea here is to use explicit metaprogramming in the sense of
;; having both ordinary Scheme semantics and stream semantics
;; available, without the language Racket #lang support from
;; stream.rkt, i.e. using the AI language as an embedding.

;; Convenience form for writing compile time combinators.
;; Semantics is passed as a dynamic parameter.
;; Note the similarity to ai-lambda, where semantics is passed as a syntax parameter.

(define ai-lambda-meta-semantics (make-parameter #f))
(define (ai-lambda-meta-op op)
  (lambda args
    (apply (ai-function-proc op)
           (ai-lambda-meta-semantics) args)))
(define-syntax ai-lambda-meta
  (syntax-rules ()
    ((_ args ((id ai-function) ... ) form)
     (make-ai-function
      (lambda (sem . args)
        (parameterize ((ai-lambda-meta-semantics sem))
          (let ((id (ai-lambda-meta-op ai-function)) ...)
            form)))
      'args))))

;; Create multi-arg versions of ai-add and ai-mul
;; These are used for `+' and `*' in stream.rkt
(define (fold-2op _op)
  (define op (ai-function-proc _op))
  (make-ai-function
   (lambda (sem first . rest)
     (let next ((args (cons first rest)))
       (let ((a  (car args))
             (as (cdr args)))
         (if (null? as) a
             (op sem a (next as))))))
   #f))
(define ai-sum  (fold-2op ai-add))
(define ai-prod (fold-2op ai-mul))

;; Flatten a nested list structure to a values list.
(define unpack
  (make-ai-function
   (lambda (sem v)
     (apply values
            (let flatten ((x v))
              (if (list? x)
                  (apply append (map flatten x))
                  (list x)))))
   '(data)))

(define unvector
  (make-ai-function
   (lambda (sem v)
     (apply values v))
   '(vector)))

(define ai-length
  (make-ai-function
   (lambda (sem v)
     (length v))
   '(vector)))


(define vector-sum
  (ai-lambda-meta
   (vec)
   ((+ ai-sum))
   (apply + vec)))
   

;; `map' works for compile-time vectors.

;; When mapping over array nodes, we have a couple of possibilities
;; for the C code gen.
;; - all array -> generate a for loop
;; - all lists -> perform map
;; - mixed: perform any combination of array->list list->array conversion
;; Converting a C-time array to a scalar list: create a list of dereferencing nodes.
;; Converting a scalar list to a C-time array: requires support in the generator.

;; FIXME: For now, it might be best to insert dereferencing nodes.
;; That would enable the important part: semantics.  Later the "for"
;; support can be added, as it is mostly an optimization.


(define ai-map
  (ai-lambda-meta
   (fn . lists)
   ((fn fn)
    (index ai-index))
   (let ((ml (min-length lists)))
     ;; FIXME: Later insert a loop + automatic primitive lifting if
     ;; none of the arguments is a compile-time list.
     (unless ml (error 'ai-map:loop-not-implemented))
     (apply map fn
            (for/list ((l lists))
              (if (list? l)
                  ;; FIXME: Pad with zeros
                  l
                  ;; Insert indices.
                  (for/list ((i (in-range ml)))
                    (index  l i ml)
                    )))))))


;; Evaluate polynomial using Horner's method
;; http://en.wikipedia.org/wiki/Horner's_method
;; a0 + x * (a1 + x * (a2 + ...))
;;
;; Coefs are in list (stream language: `vector')
;;
(define poly
  (ai-lambda-meta
   (x coefs)
   ((+ ai-add)
    (* ai-mul))
   (begin
     (let ((rcoefs (reverse coefs)))
       (for/fold
           ((acc  (car rcoefs)))
           ((coef (cdr rcoefs)))
         (+ coef (* x acc)))))))

(define (tailor-approx seq)
  (ai-lambda-meta
   (x n)
   ((poly poly))
   (if (real? n)
       (poly x (sequence-take seq n))
       (error 'tailor-approx:not-a-number (format "~s" n)))))

(define (tailor-approx-map f [n 0])
  (tailor-approx
   (sequence-map f (in-naturals n))))
  

(define (fact n)
  (if (< n 1) 1
      (* n (fact (sub1 n)))))

;; It's more useful to provide tails without the first element, or
;; with even/odd symmetry removed.

;; Expansion of f(x) : e^x = 1 + x f(x)
(define t-exp-tail (tailor-approx-map
                    (lambda (n) (/ 1 (fact n)))
                    1))

;; Expansion of f(x) : sin(x) = x * f(x^2)
;; f(q) = 1 - q/3! + q^2/5! - ...
(define t-sin-tail (tailor-approx-map
                    (lambda (n)
                      (/ (if (even? n) 1 -1)
                         (fact (+ 1 (* 2 n)))))
                    0))


;; Expansion of f(x) : cos(x) = 1 + x^2 * f(x^2)
;; f(q) = -1/2! + q/4! - q/6! + ...
(define t-cos-tail (tailor-approx-map
                    (lambda (n)
                      (/ (if (even? n) 1 -1)
                         (fact (* 2 n))))
                    1))


;; Iterate an update function a couple of times, providing the same
;; arguments except for the state.


(define iterate
  (ai-lambda-meta
   (n-in fun . args)
   ((fun fun))
   (let ((n (number->real n-in))
         (_fun
          (lambda (args)
            (values-list (apply fun args)))))
     (cond
      ((not (number? n))
       (error 'iterate:not-a-number (format "~s" n-in)))
      ((< n 0)
       (error 'iterate:negative-number (format "~s" n-in)))
      ((= n 0)
       (apply values args))
      (else
       (let* ((state0 (_fun args))
              (nstate (length state0))
              (consts (drop args nstate)))
         (apply values
                (for/fold
                    ((state state0))
                    ((_ (in-range (sub1 n))))
                  (_fun (append state consts))))))))))

;; Map-reduce (MR) is a primitive instead of a generic black-box fold.
;;
;; The MR has more structure: it exposes part of the computation as
;; parallel, while the fold is serial.  Also, an MR with an
;; associative fold operator does not need to be computed serially.
;;
;; `reduce' method for the `bus' macro.
;; (a0 a1 ... i0 i1 ...) -> (values (+ a0 i0) (+ a1 i1) ...)
(define reduce-+
  (ai-lambda-meta
   args
   ((+ ai-add))
   (let* ((state-size (/ (length args) 2))
          (as (take args state-size))
          (is (drop args state-size))
          (os (map + as is)))
     (apply values os))))
       
(define init-0
  (ai-lambda-meta
   (n)
   ((lit ai-literal))
   (apply values (for/list ((i (in-range n))) (lit 0)))))
  

    
    
;; Debug prefix                          
(define $
  (ai-lambda-meta
   (fun . args)
   ((fun      fun)
    (ai-debug ai-debug))
   (ai-debug (apply fun args))))
