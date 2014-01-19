#lang racket/base
(require racket/dict
         racket/match
         racket/pretty
         ffi/vector
         ffi/unsafe
         ffi/unsafe/define)
(provide (all-defined-out))
 
(define-ffi-definer define-rai (ffi-lib "librai"))

(define _void-pointer          (_cpointer _void))
(define _float-pointer         (_cpointer _float))
(define _float-pointer-pointer (_cpointer _float-pointer))
(define _uint32-pointer        (_cpointer _uint32))
(define _uintptr-pointer       (_cpointer _uintptr))

;; Note that these are float* and float**, but it's easier to drop the type here  (see rai.h)
(define _proc_class_run
  (_fun (_or-null _pointer) ;; state (two concatentated copies for double buffering)
        (_or-null _pointer) ;; array of input arrays
        (_or-null _pointer) ;; param array
        (_or-null _pointer) ;; array of output arrays
        (_or-null _pointer) ;; store
        _uint               ;; nb_samples
        -> _void)) 
  
(define-cstruct _proc_class_param
  ([name _string/utf-8]
   [dims _uintptr-pointer]
   [type (_enum '(float32 = 0
                  uint32  = 1
                  int32   = 2))]
   ))

(define-cstruct _proc_class_control
  ([desc  _string/utf-8]
   [unit  _string/utf-8]
   [param _proc_class_param]
   [s0    _double]
   [s1    _double]
   [range _double]
   [scale (_enum '(lin  = 0
                   log  = 1
                   slog = 2))]
   ))

(define-cstruct _proc_class
  ([magic        (_array _uint8 16)]
   [version      (_array _uint8 16)]
   [entry        _proc_class_run]
   [info_state   _proc_class_param-pointer]
   [info_in      _proc_class_param-pointer]
   [info_param   _proc_class_param-pointer]
   [info_out     _proc_class_param-pointer]
   [info_store   _proc_class_param-pointer]
   [info_control _proc_class_control-pointer]
   [init_param   _void-pointer]
   [init_state   _void-pointer]
   [init_store   _void-pointer]
   [build_stamp  _uint32]
   [__reserved   _uint32]))

(define-cstruct _proc_instance
  ([info  _proc_class-pointer]
   [state _float-pointer]
   [param _float-pointer]
   [store _float-pointer]
   ))


(define-rai rai_load_sp (_fun _string -> _proc_class-pointer))

(define-rai proc_instance_new (_fun _proc_class-pointer
                               (_or-null _proc_instance-pointer)
                               ->
                               _proc_instance-pointer))




;; Lists in rai.h are implemented using sentinel-terminated arrays,
;; where the sentinal is a 0-filled field the size of a pointer.

;; Unpack sentinel-terminated array into a list of pointers.
(define (array0->list p0 ctype)
  (let loop ((ps '())
             (i 0))
    (let ((p (ptr-add p0 i ctype)))
      (if (= 0 (ptr-ref p _uintptr))
          (reverse ps)
          (loop (cons p ps) (add1 i))))))

;; Convert info to s-expression.
(define (info-control i)
  (for/list ((p (array0->list (proc_class-info_control i) _proc_class_control)))
    `((desc  . ,(proc_class_control-desc p))
      (unit  . ,(proc_class_control-unit p))
      (param . ,(proc_class_control-param p))   ;; FIXME: unpack?
      (s0    . ,(proc_class_control-s0 p))
      (s1    . ,(proc_class_control-s1 p))
      (range . ,(proc_class_control-range p))
      (scale . ,(proc_class_control-scale p)))))

(define (info-io i info_param)
  (let* ((s-offset 0)
         (ips (array0->list (info_param i) _proc_class_param))
         (pi
          (for/list ((ip ips))
            (let* ((dims (for/list ((dp (array0->list (proc_class_param-dims ip) _uintptr)))
                           (ptr-ref dp _uintptr)))
                   (offset s-offset))
              (set! s-offset (+ s-offset (foldl * 1 dims)))
              (list (string->symbol (proc_class_param-name ip))
                    offset
                    dims)))))
    `((total . ,s-offset)
      (info . ,pi))))


(define (info-ios i)
  (for/list ((info_param (list proc_class-info_param
                               proc_class-info_in
                               proc_class-info_out
                               proc_class-info_state
                               proc_class-info_store))
             (tag '(param in out state store)))
    `(,tag . ,(info-io i info_param))))

(define (info i)
  (append
   `((control . ,(info-control i)))
   (info-ios i)))
  

;; Instantiate proc object.
;; TODO

;; How to find a good bridge between the special-cased param/buffer
;; approach and a generic scheme approach?

;; Concretely: what to do with the "param rate"?  Base "semantics"
;; doesn't have this.

;; Maybe it's best to keep this at C level?  Or maybe some duplication
;; is unavoidable..

(define (rdict-ref d tags)
  (if (null? tags)
      d
      (rdict-ref (dict-ref d (car tags)) (cdr tags))))

(define-struct proc (entry pinfo param state store nin pin nout pout) #:transparent)

(define (make/init-f32vector n [val 0.0])
  (let ((vec (make-f32vector n)))
    (for ((i (in-range n))) (f32vector-set! vec i val))
    vec))

(define (proc-instantiate i [defaults '()])
  (let* ((info (info i))
         (total (lambda (tag) (rdict-ref info `(,tag total))))
         (nin  (total 'in))
         (nout (total 'out))
         (param (make/init-f32vector (total 'param)))
         (state (make/init-f32vector (* 2 (total 'state))))
         (store (make/init-f32vector (total 'store)))
         (p
          (make-proc
           (proc_class-entry i)
           (rdict-ref info '(param info))
           param state store
           nin  (malloc _float-pointer nin)
           nout (malloc _float-pointer nout))))
    (for (((k v) (in-dict defaults)))
      (proc-param-set! p k v))
    p))
       

         
;; FIXME: use librai methods.
(define (proc-param-set! p param val)
  (unless (number? param)
    (set! param (car (dict-ref (proc-pinfo p) param))))
  (when param
    (f32vector-set! (proc-param p) param val)))
  

(define (ptr-set-f32vectors! arr vs)
  (for ((i (in-naturals)) (v vs))
    (ptr-set! arr _pointer i (f32vector->cpointer v))))



(define (proc-run p ins/n [outs #f])
  (let-values
      (((n ins)
        (if (number? ins/n)
            (values ins/n '())
            (values (f32vector-length (car ins/n)) ins/n))))
    (when (odd? n)
      (error 'odd-run-not-supported)) ;; need to copy state buffer!
    (match p
      ((struct proc (entry pinfo param state store nin pin nout pout))
       (unless outs 
         (set! outs (for/list ((i nout)) (make-f32vector n))))
       (ptr-set-f32vectors! pin  ins)
       (ptr-set-f32vectors! pout outs)
       (entry (f32vector->cpointer state) pin
              (f32vector->cpointer param) pout
              (f32vector->cpointer store) n)
       outs))))

       
     

