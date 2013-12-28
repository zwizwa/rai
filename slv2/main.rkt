#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         "lv2.rkt")
(define-ffi-definer define-slv2 (ffi-lib "libslv2" "9"))
(provide (all-defined-out))

;; HACK: preload for swh-lv2
(define-ffi-definer define-fftw (ffi-lib "libfftw"))


;; libslv2 API doc is here:
;; http://drobilla.net/docs/slv2/
;;
;; Other info:
;; http://drobilla.net/software/slv2/
;; http://home.gna.org/zynjacku/

;; Check documentation before use of slv2_value_free; the ownership
;; rules are ad-hoc.

;; The SLV2PluginInstance functions are static inline, so are
;; re-implemented here based on definitions in lv2/plugininstance.h

(define _slv2_instanceimpl (_cpointer 'SLV2InstanceImpl))


;; Mixing incomplete types (_cpointer) and complete types
;; (define-cstruct) can be confusing because of the <ID> and
;; <ID>-pointer naming scheme, but it doesn't seem that it merits the
;; punishment of properly abstracted objects with notational overhead
;; in the form of a -pointer postfix.

;; ( Writing a C FFI removes any residual love for complete types and
;; static inline functions... )

(define-cstruct _slv2_instance
  ([lv2_descriptor _lv2_descriptor-pointer]
   [lv2_handle     _lv2_handle]
   [pimpl          _slv2_instanceimpl]))



(define _slv2_world    (_cpointer 'SLV2World))
(define _slv2_plugin   (_cpointer 'SLV2Plugin))
(define _slv2_plugins  (_cpointer 'SLV2Plugins))
(define _slv2_value    (_cpointer 'SLV2Value))
(define _slv2_values   (_or-null (_cpointer 'SLV2Values)))
(define _slv2_port     (_cpointer 'SLV2Port))

(define-slv2 slv2_world_new             (_fun -> _slv2_world))
(define-slv2 slv2_world_load_all        (_fun _slv2_world -> _void))
(define-slv2 slv2_world_get_all_plugins (_fun _slv2_world -> _slv2_plugins))

(define-slv2 slv2_plugins_size   (_fun _slv2_plugins -> _uint))
(define-slv2 slv2_plugins_get_at (_fun _slv2_plugins _uint -> _slv2_plugin))

(define-slv2 slv2_plugin_get_name          (_fun _slv2_plugin -> _slv2_value))
(define-slv2 slv2_plugin_get_num_ports     (_fun _slv2_plugin -> _uint))
(define-slv2 slv2_plugin_get_port_by_index (_fun _slv2_plugin _uint -> _slv2_port))
(define-slv2 slv2_plugin_instantiate       (_fun _slv2_plugin _double _lv2_features -> _slv2_instance-pointer))

(define-slv2 slv2_value_free      (_fun _slv2_value -> _void))
(define-slv2 slv2_value_as_string (_fun _slv2_value -> _string/utf-8))

(define-slv2 slv2_value_new_string (_fun _slv2_world _string/utf-8 -> _slv2_value))
(define-slv2 slv2_value_new_float  (_fun _slv2_world _float        -> _slv2_value))
(define-slv2 slv2_value_new_int    (_fun _slv2_world _int          -> _slv2_value))
#;(define-slv2 slv2_value_new_bool   (_fun _slv2_world _bool         -> _slv2_value))


(define-slv2 slv2_values_size     (_fun _slv2_values -> _uint))
(define-slv2 slv2_values_get_at   (_fun _slv2_values _uint -> _slv2_value))

(define-slv2 slv2_port_get_name       (_fun _slv2_plugin _slv2_port -> _slv2_value))
(define-slv2 slv2_port_get_properties (_fun _slv2_plugin _slv2_port -> _slv2_values))
(define-slv2 slv2_port_is_a 	      (_fun _slv2_plugin _slv2_port _slv2_value -> _bool))
(define-slv2 slv2_port_get_classes    (_fun _slv2_plugin _slv2_port -> _slv2_values))
