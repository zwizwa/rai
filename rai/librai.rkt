#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(provide (all-defined-out))
 
(define-ffi-definer define-rai (ffi-lib "librai"))

(define _float-pointer         (_cpointer _float))
(define _float-pointer-pointer (_cpointer _float-pointer))
(define _uint32-pointer        (_cpointer _uint32))

(define _rai_info_run
  (_fun _float-pointer          ;; state (double buffered)
        _float-pointer-pointer  ;; array of input arrays
        _float-pointer          ;; param array
        _float-pointer-pointer  ;; array of output arrays
        _float-pointer          ;; store
        _uint                   ;; nb_samples
        -> _void)) 
  
(define-cstruct _rai_info_param
  ([name _string/utf-8]
   [dims _uint32-pointer]))

(define-cstruct _rai_info_control
  ([desc  _string/utf-8]
   [unit  _string/utf-8]
   [index _int]
   [s0    _float]
   [s1    _float]
   [range _float]
   [scale (_enum '(rai_scale_lin  = 0
                   rai_scale_log  = 1
                   rai_scale_slog = 2))]
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

;; Might seem a bit annoying, but actually avoiding a separate size
;; field makes data structures easer to read from Scheme, and easier
;; to generate from C.


;; Unpack sentinel-terminated array into a list of pointers.
(define (array0->list p0 ctype)
  (let loop ((ps '())
             (i 0))
    (let ((p (ptr-add p0 i ctype)))
      (if (= 0 (ptr-ref p _uintptr))
          (reverse ps)
          (loop (cons p ps) (add1 i))))))
  
