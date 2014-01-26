#lang racket/base
(require "tools.rkt"
         "libproc.rkt"
         racket/runtime-path
         ffi/vector
         ffi/unsafe
         ffi/unsafe/define)




(define (test test.sp)

  (pp test.sp)

  (define test_sp_info (proc_load_sp test.sp))
  (define test_sp_proc (proc_instance_new test_sp_info #f))


  (define v (make-f32vector 16))
  
  (pp test_sp_proc)

  (define test_param (proc_class-info_param test_sp_info))

  (pp test_param)


  (define (proc_class->name i n)
    (map proc_class_param-name (array0->list (proc_class-info_param i))))


  (define param-names
    (map proc_class_param-name (array0->list (proc_class-info_param test_sp_info) _proc_class_param)))
  
  (define control-names
    (map proc_class_control-desc (array0->list (proc_class-info_control test_sp_info) _proc_class_control)))

  (pp param-names)
  (pp control-names)

  (pp (info test_sp_info))

  test_sp_info)

;; FIXME: where to store the binaries? currently they're in the source dir.
(define-runtime-path test_pd.sp "test_pd.sp")
(define-runtime-path synth.sp   "synth.sp")

(define test_pd_class    (test test_pd.sp))
(define test_synth_class (test synth.sp))
  
(define test_pd_instance
  (proc-instantiate test_pd_class
                    '((samplerate . 1.0)
                      (voice_gate . 1.0)
                      (voice_freq . 0.2))))
(map f32vector->list (proc-run test_pd_instance 10))


;; (test-run)

(define test_synth_instance
  (proc-instantiate test_synth_class
                    '((samplerate . 1)
                      (timestep   . 1))))
(map f32vector->list (proc-run test_synth_instance 10))

;; (define i (proc-instantiate c))


