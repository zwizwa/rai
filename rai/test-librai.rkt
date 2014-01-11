#lang racket/base
(require "librai.rkt"
         racket/runtime-path
         ffi/vector
         ffi/unsafe
         ffi/unsafe/define)

;; FIXME: where to store the binaries? currently they're in the source dir.
(define-runtime-path test_pd.sp "test_pd.sp")


test_pd.sp

(define test_sp_info (rai_load_bin test_pd.sp))
(define test_sp_proc (rai_proc_new test_sp_info #f))


(define v (make-f32vector 16))

test_sp_proc

(define test_param (rai_info-info_param test_sp_info))

test_param


(define (rai_info->name i n)
  (map rai_info_param-name (array0->list (rai_info-info_param i))))


(define param-names
  (map rai_info_param-name (array0->list (rai_info-info_param test_sp_info) _rai_info_param)))

(define control-names
  (map rai_info_control-desc (array0->list (rai_info-info_control test_sp_info) _rai_info_control)))

param-names
control-names
