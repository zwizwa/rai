#lang racket/base
(require "librai.rkt"
         ffi/unsafe
         ffi/unsafe/define)


(define test_sp_info (rai_load_bin "test_pd.sp"))
(define test_sp_proc (rai_proc_new test_sp_info 0))

(define _sig_block (_array _float 16))
(define o-mem (malloc _sig_block))
(define o (ptr-ref o-mem _sig_block 0))

(array-set! o 0 123.0)
(define o0 (array-ref o 0))
