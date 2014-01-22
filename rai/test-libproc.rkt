#lang racket/base
(require "libproc.rkt"
         racket/runtime-path
         ffi/vector
         ffi/unsafe
         ffi/unsafe/define)

;; FIXME: where to store the binaries? currently they're in the source dir.
(define-runtime-path test_pd.sp "test_pd.sp")
;; (define-runtime-path test_pd.sp "synth.sp")


test_pd.sp

(define test_sp_info (proc_load_sp test_pd.sp))
(define test_sp_proc (proc_instance_new test_sp_info #f))


(define v (make-f32vector 16))

test_sp_proc

(define test_param (proc_class-info_param test_sp_info))

test_param


(define (proc_class->name i n)
  (map proc_class_param-name (array0->list (proc_class-info_param i))))


(define param-names
  (map proc_class_param-name (array0->list (proc_class-info_param test_sp_info) _proc_class_param)))

(define control-names
  (map proc_class_control-desc (array0->list (proc_class-info_control test_sp_info) _proc_class_control)))

param-names
control-names

(info test_sp_info)


#;(define i (proc-instantiate test_sp_info
                            '((samplerate . 1.0)
                              (voice_gate . 1.0)
                              (voice_freq . 0.2))))
#;(map f32vector->list (proc-run i 10))
