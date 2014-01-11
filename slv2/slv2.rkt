#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         "lv2.rkt")
(define-ffi-definer define-slv2 (ffi-lib "libslv2" "9"))
(provide (all-defined-out))

;; HACK: preload for swh-lv2
(define-ffi-definer define-fftw (ffi-lib "libfftw" "2"))


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

(define _slv2_instanceimpl-pointer (_cpointer 'SLV2InstanceImpl))


(define-cstruct _slv2_instance
  ([lv2_descriptor _lv2_descriptor-pointer]
   [lv2_handle     _lv2_handle-pointer]
   [pimpl          _slv2_instanceimpl-pointer]))


(define _slv2_world-pointer    (_cpointer 'SLV2World))
(define _slv2_plugin-pointer   (_cpointer 'SLV2Plugin))
(define _slv2_plugins-pointer  (_cpointer 'SLV2Plugins))
(define _slv2_value-pointer    (_cpointer 'SLV2Value))
(define _slv2_values-pointer   (_or-null (_cpointer 'SLV2Values)))
(define _slv2_port-pointer     (_cpointer 'SLV2Port))

(define-slv2 slv2_world_new             (_fun -> _slv2_world-pointer))
(define-slv2 slv2_world_load_all        (_fun _slv2_world-pointer -> _void))
(define-slv2 slv2_world_get_all_plugins (_fun _slv2_world-pointer -> _slv2_plugins-pointer))

(define-slv2 slv2_plugins_size   (_fun _slv2_plugins-pointer -> _uint))
(define-slv2 slv2_plugins_get_at (_fun _slv2_plugins-pointer _uint -> _slv2_plugin-pointer))

(define-slv2 slv2_plugin_get_name          (_fun _slv2_plugin-pointer -> _slv2_value-pointer))
(define-slv2 slv2_plugin_get_num_ports     (_fun _slv2_plugin-pointer -> _uint))
(define-slv2 slv2_plugin_get_port_by_index (_fun _slv2_plugin-pointer _uint -> _slv2_port-pointer))
(define-slv2 slv2_plugin_instantiate       (_fun _slv2_plugin-pointer _double _lv2_features-pointer -> _slv2_instance-pointer))

(define-slv2 slv2_value_free      (_fun _slv2_value-pointer -> _void))
(define-slv2 slv2_value_as_string (_fun _slv2_value-pointer -> _string/utf-8))

(define-slv2 slv2_value_new_string (_fun _slv2_world-pointer _string/utf-8 -> _slv2_value-pointer))
(define-slv2 slv2_value_new_float  (_fun _slv2_world-pointer _float        -> _slv2_value-pointer))
(define-slv2 slv2_value_new_int    (_fun _slv2_world-pointer _int          -> _slv2_value-pointer))
#;(define-slv2 slv2_value_new_bool   (_fun _slv2_world _bool         -> _slv2_value-pointer))


(define-slv2 slv2_values_size     (_fun _slv2_values-pointer -> _uint))
(define-slv2 slv2_values_get_at   (_fun _slv2_values-pointer _uint -> _slv2_value-pointer))

(define-slv2 slv2_port_get_name       (_fun _slv2_plugin-pointer _slv2_port-pointer -> _slv2_value-pointer))
(define-slv2 slv2_port_get_properties (_fun _slv2_plugin-pointer _slv2_port-pointer -> _slv2_values-pointer))
(define-slv2 slv2_port_is_a 	      (_fun _slv2_plugin-pointer _slv2_port-pointer _slv2_value-pointer -> _bool))
(define-slv2 slv2_port_get_classes    (_fun _slv2_plugin-pointer _slv2_port-pointer -> _slv2_values-pointer))
