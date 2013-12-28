#lang racket
(require
 ffi/vector
 "main.rkt"
 "lv2.rkt")
         

(provide plugins
         plugin-ports
         plugin-name
         plugin-port-name
         plugin-instantiate)

;; Sugar on top of the raw libslv2 C API.
;; API is UNSTABLE.

(define world (slv2_world_new))

(slv2_world_load_all world)

(define input   (slv2_value_new_string world "input"))
(define output  (slv2_value_new_string world "output"))
(define audio   (slv2_value_new_string world "audio"))
(define control (slv2_value_new_string world "control"))
(define midi    (slv2_value_new_string world "midi"))


;; Avoid the iterator interface - just use Racket lists for collections.
(define (get-list obj get_size ref)
  (let ((n (get_size obj)))
    ;; (pretty-print `(n ,n))
    (for/list ((i (in-range n)))
      (ref obj i))))

;; Handle the freeing of the SDLValue object before copy to Racket string.
(define (->string/free value)
  (let* ((str (string-append "" (slv2_value_as_string value)))) ;; copy
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

(define (values->list vs)
  (if (not vs) '() ;; For NULL
      (get-list vs
                slv2_values_size
                slv2_values_get_at)))
            

(define (plugin-name plugin) (->string/free (slv2_plugin_get_name plugin)))
(define (plugin-port-name plugin port) (->string/free (slv2_port_get_name plugin port)))

(define (plugin-port-names plugin)
  (for/list ((port (plugin-ports plugin)))
    (plugin-port-name plugin port)))

(define (plugin-port-properties plugin port)
  (values->list (slv2_port_get_properties plugin port)))

(define (plugin-instantiate plugin samplerate)
  (slv2_plugin_instantiate plugin samplerate #f))

(define (plugin-port-classes plugin port)
  (map slv2_value_as_string
       (values->list (slv2_port_get_classes plugin port))))

;; Serves as an example of how to pierce through the indirections.
(define (plugin-instance-connect-port-f32vector instance index vector)
  (let* ((descriptor (slv2_instance-lv2_descriptor instance))    ;; class
         (handle     (slv2_instance-lv2_handle     instance))    ;; object
         (connect    (lv2_descriptor-connect_port  descriptor))) ;; method
    (connect handle index (f32vector->cpointer vector))))
     
;; --------------------------------------------------------------------------------------

(define (test)
  (test-plugin (car (plugins))))
(define (test-plugin p)
  (let* ((i (plugin-instantiate p 44100.0))
         (d (slv2_instance-lv2_descriptor i))
         (h (slv2_instance-lv2_handle i))
         (ports (plugin-ports p))
         (iobuf (make-f32vector 1)))
    (plugin-instance-connect-port-f32vector i 0 iobuf)
    (pretty-print
     `(,(plugin-name p)
       ,(plugin-port-names p)
       ,(lv2_descriptor-URI d)
       ,(for/list ((port ports))
          (list (plugin-port-properties p port)
                (plugin-port-classes p port)))))))

;; Instantiation might fail, e.g. due to linking errors.     
(define (instantiate-working-plugins [sr 48000.0])
  (define is '())
  (for ((p (plugins)))
    (with-handlers ((void void))
      (set! is (cons (cons p (plugin-instantiate p sr)) is))))
  (reverse is))

