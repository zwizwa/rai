#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(provide (all-defined-out))
 
(define-ffi-definer define-rai (ffi-lib "librai"))

(define _rai_info-pointer (_cpointer 'rai_info))
(define-rai rai_load_bin (_fun _string -> _rai_info-pointer))

(define _rai_proc-pointer (_cpointer 'rai_proc))
(define-rai rai_proc_new (_fun _rai_info-pointer _int -> _rai_proc-pointer))

