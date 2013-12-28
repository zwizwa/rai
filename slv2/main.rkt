#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(define-ffi-definer define-slv2 (ffi-lib "libslv2" "9"))
(provide (all-defined-out))

;; http://drobilla.net/software/slv2/
;; http://drobilla.net/docs/slv2/
;; http://home.gna.org/zynjacku/
(define _slv2_world   (_cpointer 'SLV2World))
(define _slv2_plugin  (_cpointer 'SLV2Plugin))
(define _slv2_plugins (_cpointer 'SLV2Plugins))
(define _slv2_value   (_cpointer 'SLV2Value))
(define _slv2_port    (_cpointer 'SLV2Port))

(define-slv2 slv2_world_new (_fun -> _slv2_world))
(define-slv2 slv2_world_load_all (_fun _slv2_world -> _void))
(define-slv2 slv2_world_get_all_plugins (_fun _slv2_world -> _slv2_plugins))

(define-slv2 slv2_plugins_size   (_fun _slv2_plugins -> _uint))
(define-slv2 slv2_plugins_get_at (_fun _slv2_plugins _uint -> _slv2_plugin))

(define-slv2 slv2_plugin_get_name (_fun _slv2_plugin -> _slv2_value))
(define-slv2 slv2_plugin_get_num_ports (_fun _slv2_plugin -> _uint))
(define-slv2 slv2_plugin_get_port_by_index (_fun _slv2_plugin _uint -> _slv2_port))
  
;; Check documentation before use of slv2_value_free
;; The ownership rules are quite ad-hoc.
(define-slv2 slv2_value_free (_fun _slv2_value -> _void))
(define-slv2 slv2_value_as_string (_fun _slv2_value -> _bytes))

(define-slv2 slv2_port_get_name (_fun _slv2_plugin _slv2_port -> _slv2_value))


