#lang racket/base
(require racket/dict
         racket/match
         racket/pretty
         ffi/vector
         ffi/unsafe
         ffi/unsafe/define
         "f32vector.rkt")

(provide (all-defined-out))
 
(define-ffi-definer define-proc (ffi-lib "libproc"))

(define _void-pointer          (_cpointer _void))
(define _float-pointer         (_cpointer _float))
(define _float-pointer-pointer (_cpointer _float-pointer))
(define _uint32-pointer        (_cpointer _uint32))
(define _uintptr-pointer       (_cpointer _uintptr))
(define _int-pointer           (_cpointer _int))

;; FIXME: make sure libproc.so is compiled with PROC_NUMBER_T = double
(define _number                _double)

;; We don't know the types at compile time.  It is necessary to proc.h
;; metadata to reconstruct data layout information at run time.
(define _proc_class_run
  (_fun (_or-null _pointer) ;; state (two concatentated copies for double buffering)
        (_or-null _pointer) ;; array of input arrays
        (_or-null _pointer) ;; param array
        (_or-null _pointer) ;; array of output arrays
        (_or-null _pointer) ;; store
        _uint               ;; nb_samples
        -> _void))

(define _proc_type (_enum '(float32 = 0
                            uint32  = 1
                            int32   = 2)))
  
(define-cstruct _proc_class_param
  ([name _string/utf-8]
   [dims _uintptr-pointer]
   [type _proc_type]))

(define _proc_scale (_enum '(lin  = 0
                             log  = 1
                             slog = 2)))

(define-cstruct _proc_class_control_map
  ([s0    _double]
   [s1    _double]
   [range _double]
   [scale _proc_scale]))


(define-cstruct _proc_class_control
  ([desc  _string/utf-8]
   [unit  _string/utf-8]
   [param _proc_class_param-pointer]
   [map   _proc_class_control_map]))

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
   [init_state   _void-pointer]
   [init_store   _void-pointer]
   [build_stamp  _uint32]))

(define-cstruct _proc_instance
  ([info         _proc_class-pointer]
   [state        _float-pointer]
   [param        _float-pointer]
   [store        _float-pointer]
   [param_offset _int-pointer]
   [param_nb_el  _int-pointer]
   [nb_control   _int]
   [size_state   _int]
   [size_param   _int]
   [size_store   _int]
   ))


(define-proc proc_load_sp
  (_fun _string -> _proc_class-pointer))

(define-proc proc_class_param_alloc_size (_fun _proc_class_param-pointer -> _int))
(define-proc proc_class_param_list_size  (_fun _proc_class_param-pointer -> _int))


(define-proc proc_instance_new
  (_fun _proc_class-pointer
        (_or-null _proc_instance-pointer)
        ->
        _proc_instance-pointer))

(define-proc proc_instance_free          (_fun _proc_instance-pointer -> _void))
(define-proc proc_instance_nb_control    (_fun _proc_instance-pointer -> _int))
(define-proc proc_instance_reset_state   (_fun _proc_instance-pointer -> _void))
(define-proc proc_instance_find_param    (_fun _proc_instance-pointer _string/utf-8 -> _int))
(define-proc proc_instance_find_control  (_fun _proc_instance-pointer _int -> _int))

(define-proc proc_instance_set_param     (_fun _proc_instance-pointer _int _number -> _void))
(define-proc proc_instance_get_param     (_fun _proc_instance-pointer _int -> _number))

(define-proc proc_instance_run
  (_fun _proc_instance-pointer
        (_or-null _void-pointer)
        (_or-null _void-pointer)
        _int -> _void))





;; Lists in proc.h are implemented using sentinel-terminated arrays,
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
    (let ((m (proc_class_control-map p)))
      `((desc  . ,(proc_class_control-desc p))
        (unit  . ,(proc_class_control-unit p))
        (param . ,(proc_class_control-param p))   ;; FIXME: unpack?
        (s0    . ,(proc_class_control_map-s0 m))
        (s1    . ,(proc_class_control_map-s1 m))
        (range . ,(proc_class_control_map-range m))
        (scale . ,(proc_class_control_map-scale m))))))

(define (info-io i info_param)
  (let* ((ips (array0->list (info_param i) _proc_class_param)))
    (for/list ((ip ips))
      (let* ((dims (for/list ((dp (array0->list (proc_class_param-dims ip) _uintptr)))
                     (ptr-ref dp _uintptr))))
        (list (string->symbol (proc_class_param-name ip))
              dims)))))

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

(define-struct proc (instance
                     nin  pin
                     nout pout) #:transparent)

(define (proc-set-param! p name value)
  (let* ((instance (proc-instance p))
         (index (proc_instance_find_param instance (symbol->string name))))
    (when (< index 0) (error name))
    (proc_instance_set_param instance index (+ 0.0 value))))
  

(define (proc-instantiate class [defaults '()])
  (let* ((nin   (proc_class_param_list_size (proc_class-info_in  class)))
         (nout  (proc_class_param_list_size (proc_class-info_out class)))
         (p     (make-proc (proc_instance_new class #f)
                           nin  (if (zero? nin)  #f (malloc _float-pointer nin))
                           nout (if (zero? nout) #f (malloc _float-pointer nout)))))
    (for (((name value) (in-dict defaults)))
      (proc-set-param! p name value))
    p))

         

(define (proc-run p ins/n [outs #f])
  (let-values
      (((n ins)
        (if (number? ins/n)
            (values ins/n '())
            (values (f32vector-length (car ins/n)) ins/n))))
    (match p
      ((struct proc (instance nin pin nout pout))
       (unless outs 
         (set! outs (for/list ((i nout)) (make-f32vector n))))
       (ptr-set-f32vectors! pin  ins)
       (ptr-set-f32vectors! pout outs)
       (proc_instance_run instance pin pout n)
       outs))))

       
     
