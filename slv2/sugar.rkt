#lang racket
(require "main.rkt")

(provide plugins
         plugin-ports
         plugin-name
         plugin-port-name)

;; Sugar on top of the raw libslv2 C API.
;; API is UNSTABLE.

(define world (slv2_world_new))

(slv2_world_load_all world)

;; Avoid the iterator interface - just use Racket lists for collections.
(define (get-list obj get_size ref)
  (for/list ((i (in-range (get_size obj))))
    (ref obj i)))

;; Handle the freeing of the SDLValue object before copy to Racket string.
(define (->string/free value)
  (let* ((str (bytes->string/utf-8 (slv2_value_as_string value))))
    (slv2_value_free value)
    str))


(define (plugins)
  (let ((ps (slv2_world_get_all_plugins world)))
    (get-list ps
              slv2_plugins_size
              slv2_plugins_get_at)))

(define (plugin-ports plugin)
  (get-list plugin
            slv2_plugin_get_num_ports
            slv2_plugin_get_port_by_index))

(define (plugin-name plugin) (->string/free (slv2_plugin_get_name plugin)))
(define (plugin-port-name plugin port) (->string/free (slv2_port_get_name plugin port)))

(define (plugin-port-names plugin)
  (for/list ((port (plugin-ports plugin)))
    (plugin-port-name plugin port)))

