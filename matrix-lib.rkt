#lang racket/base
(require "tools.rkt"
         "stream-syntax.rkt")
(provide (all-defined-out))

;; AI-friendly matrix routines.

;; (split-matrix 2 2 '((1  2  3  4)
;;                     (5  6  7  8)
;;                     (9 10 11 12)))
;;
;; => (( ((1 2)
;;        (5 6))
;;
;;       ((3 4)
;;        (7 8)) )
;;
;;     ( ((9 10))
;;
;;     ( (11 12)) ))

(define (matrix-split n m mat)
  (define (split-list x)
    (lambda (lst)
      (list (take lst x)
            (drop lst x))))
  ;; Grid Forth ;)
  (transpose
   (map (split-list m)
        (transpose
         (map (split-list n) mat)))))

(define (matrix-eye n [v 1])
  (for/list ((r n))
    (for/list ((c n))
      (if (= r c) v 0))))

(define (matrix-ref m row col)
  (list-ref (list-ref m row) col))


;; Matrix multiplication.  For the vec-mul it's possible to use a
;; binary tree approach instead of a linear list approach: that will
;; decouple dependencies a bit leaving more room for parallel
;; optimizations.  Might be that -ffast-math already does this.
(define (ai-vec-mul sem va vb)
  (define (+ a b) ((ai-impl-add sem) sem a b))
  (define (* a b) ((ai-impl-mul sem) sem a b))
  (for/fold
      ((acc (* (car va) (car vb))))
      ((a (cdr va)) (b (cdr vb)))
    (+ acc (* a b))))

(define (ai-matrix-mul sem a b)
  (let ((b (transpose b)))
    (for/list ((ra a))
    (for/list ((rb b))
      (ai-vec-mul sem ra rb)))))

(define (vector->col-matrix v) (map list v))
(define (col-matrix->vector m) (map car m))

(define (ai-matrix-vector-mul sem m v)
  (col-matrix->vector
   (ai-matrix-mul sem m (vector->col-matrix v))))

(define (lift-matrix prim)
  (lambda (sem a b)
    (for/list ((ra a) (rb b))
      (for/list ((ea ra) (eb rb))
        ((prim sem) sem ea eb)))))

(define ai-matrix-add (lift-matrix ai-impl-add))
(define ai-matrix-sub (lift-matrix ai-impl-sub))



(define (mutable-matrix mat #:extra-cols [extra-cols #f])
  (let ((m
         (list->vector
          (map (lambda (l)
                 (list->vector
                  (append l (make-list extra-cols 0))))
               mat))))
    (define (m@ r c)   (vector-ref  (vector-ref m r) c))
    (define (m! r c v) (vector-set! (vector-ref m r) c v))
    (values m@ m!)))
    
;; Matrix inverse.  Dumb, non-pivoting GE.

(define (ai-matrix-inverse sem mat)
  (define (fun proc)
    (lambda args (apply (proc sem) sem args)))

  (define literal (fun ai-impl-literal))
  (define zero (literal 0))
  (define one  (literal 1))
  (define (zero? x) (eq? zero x))
  (define + (fun ai-impl-add))
  (define - (fun ai-impl-sub))
  (define * (fun ai-impl-mul))
  (define / (fun ai-impl-div))
  
  (let* ((rows (length mat))
         (cols (length (car mat)))
         (all-cols (* 2 cols))
         (_ (unless (= rows cols) (error 'not-square))))
    (define-values
      (m@ m!)
      (mutable-matrix mat #:extra-cols (- all-cols cols)))


    ;; Row operations.  Note that the algo below is dumb in that it
    ;; doesn't take into account that elements might be 1 or 0.  The
    ;; point is to generate code that will be partially evaluated.
    
    (define (scale-sub! ra rb scale)
      (for ((c all-cols))
        (m! ra c (- (m@ ra c) (* scale (m@ rb c))))))
    (define (scale-row! r scale)
      (for ((c all-cols))
        (m! r c (* scale (m@ r c)))))
    
    ;; Set extended part to identity matrix.
    (for ((c cols))
    (for ((r rows))
      (m! r (+ c cols)
          (if (= r c) one zero))))

    ;; GE
    (for ((d cols))  ;; diagonal
      (let ((pivot (m@ d d)))
        (when (zero? pivot) (error 'zero-pivot))
        ;; Scale pivot row
        (scale-row! d (/ one pivot))
        (m! d d one)) ;; Set exact one
    
      ;; Perform elementary operations on the other rows.
      (for ((r rows)
            #:unless (= r d))
        (let ((head (m@ r d)))
          (scale-sub! r d head)
          (m! r d one)))
      )
      
    
    (for/list ((r rows))
    (for/list ((c (in-range cols all-cols)))
      (m@ r c)))

    ))


    
        
    