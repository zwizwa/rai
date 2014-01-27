#lang racket/base
(require "tools.rkt"
         "libproc.rkt"
         racket/runtime-path
         ffi/vector
         ffi/unsafe
         ffi/unsafe/define)




(define (test test.sp)

  (pp test.sp)

  (define test_sp_class (proc_load_sp test.sp))

  (pp (proc-class->dict test_sp_class))

  #;(begin

  (define v (make-f32vector 16))
  

  (define test_param (proc_class-info_param test_sp_class))

  (pp test_param)


  (define test_sp_proc (proc_instance_new test_sp_class #f))
  (pp test_sp_proc)
  
  (define (proc_class->name i n)
    (map proc_class_param-name (array0->list (proc_class-info_param i))))


  (define param-names
    (map proc_class_param-name (array0->list (proc_class-info_param test_sp_class) _proc_class_param)))
  
  (define control-names
    (map proc_class_control-desc (array0->list (proc_class-info_control test_sp_class) _proc_class_control)))

  (pp param-names)
  (pp control-names)

  (pp (info test_sp_class))
  )

  test_sp_class)

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
(map f32vector->list (proc-run! test_pd_instance 10))


;; (test-run)

(define test_synth_instance
  (proc-instantiate test_synth_class
                    '((samplerate . 1)
                      (timestep   . 1))))
(map f32vector->list (proc-run! test_synth_instance 10))

;; (define i (proc-instantiate c))


