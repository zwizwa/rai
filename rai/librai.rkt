#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(provide (all-defined-out))
 
(define-ffi-definer define-rai (ffi-lib "librai"))

(define _float-pointer         (_cpointer _float))
(define _float-pointer-pointer (_cpointer _float-pointer))
(define _uint32-pointer        (_cpointer _uint32))
(define _uintptr-pointer       (_cpointer _uintptr))

(define _rai_info_run
  (_fun _float-pointer          ;; state (two concatentated copies for double buffering)
        _float-pointer-pointer  ;; array of input arrays
        _float-pointer          ;; param array
        _float-pointer-pointer  ;; array of output arrays
        _float-pointer          ;; store
        _uint                   ;; nb_samples
        -> _void)) 
  
(define-cstruct _rai_info_param
  ([name _string/utf-8]
   [dims _uintptr-pointer]))

(define-cstruct _rai_info_control
  ([desc  _string/utf-8]
   [unit  _string/utf-8]
   [index _int]
   [s0    _float]
   [s1    _float]
   [range _float]
   [scale (_enum '(lin  = 0
                   log  = 1
                   slog = 2))]
   ))

(define-cstruct _rai_info
  ([magic        (_array _uint8 16)]
   [version      (_array _uint8 16)]
   [entry        _rai_info_run]
   [info_state   _rai_info_param-pointer]
   [info_in      _rai_info_param-pointer]
   [info_param   _rai_info_param-pointer]
   [info_out     _rai_info_param-pointer]
   [info_store   _rai_info_param-pointer]
   [info_control _rai_info_control-pointer]
   [build_stamp  _uint32]
   [__reserved   _uint32]))

(define-cstruct _rai_proc
  ([info  _rai_info-pointer]
   [state _float-pointer]
   [param _float-pointer]
   [store _float-pointer]
   ))


(define-rai rai_load_bin (_fun _string -> _rai_info-pointer))

(define-rai rai_proc_new (_fun _rai_info-pointer
                               (_or-null _rai_proc-pointer)
                               ->
                               _rai_proc-pointer))




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
  (for/list ((p (array0->list (rai_info-info_control i) _rai_info_control)))
    `((desc  . ,(rai_info_control-desc p))
      (unit  . ,(rai_info_control-unit p))
      (index . ,(rai_info_control-index p))
      (s0    . ,(rai_info_control-s0 p))
      (s1    . ,(rai_info_control-s1 p))
      (range . ,(rai_info_control-range p))
      (scale . ,(rai_info_control-scale p)))))

(define (info-io i info_param)
  (let* ((s-offset 0)
         (ips (array0->list (info_param i) _rai_info_param))
         (pi
          (for/list ((ip ips))
            (let* ((dims (for/list ((dp (array0->list (rai_info_param-dims ip) _uintptr)))
                           (ptr-ref dp _uintptr)))
                   (offset s-offset))
              (set! s-offset (+ s-offset (foldl * 1 dims)))
              `((name . ,(rai_info_param-name ip))
                (dims . ,dims)
                (offset . ,offset))))))
    `((total . ,s-offset)
      (info . ,pi))))


(define (info-ios i)
  (for/list ((info_param (list rai_info-info_param
                               rai_info-info_in
                               rai_info-info_out
                               rai_info-info_state
                               rai_info-info_store))
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

(define (run i)
  (let ((r (rai_info-entry i)))
    r))
