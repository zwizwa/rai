#lang racket/base
;; Core LV2 independent of libslv2.
(require ffi/unsafe
         ffi/vector)
(provide (all-defined-out))


(define _lv2_features-pointer   (_or-null (_cpointer 'LV2Features)))
(define _lv2_handle-pointer     (_cpointer 'LV2_Handle))

;; The type of this is defined in the metadata.
;; audio   : array of floats
;; control : single float
(define _lv2_port_data-pointer  _pointer)

(define-cstruct _lv2_descriptor
  ([URI            _string/utf-8]
   ;; You probably want to use slv2_plugin_instantiate instead.
   [instantiate    (_fun _lv2_descriptor-pointer
                         _double               ;; sample rate
                         _string/utf-8         ;; bundle path
                         _lv2_features-pointer ;; features
                         ->
                         _lv2_handle-pointer)]
   [connect_port   (_fun _lv2_handle-pointer _uint _lv2_port_data-pointer -> _void)]
   [activate       (_fun _lv2_handle-pointer -> _void)]
   [run            (_fun _lv2_handle-pointer _uint -> _void)]
   [deactivate     (_fun _lv2_handle-pointer -> _void)]
   [cleanup        (_fun _lv2_handle-pointer -> _void)]
   [extension_data (_fun _lv2_handle-pointer _string/utf-8 -> _bytes)]
   
   ))

  


