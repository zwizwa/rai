#lang racket/base
(require racket/system)
(provide (all-defined-out))

;; FIXME: auto config
;(define g++ "/usr/bin/i586-mingw32msvc-g++")
;(define vst "/home/tom/kmook/vst/vstsdk2.4")

(define g++ "C:/MinGW/bin/mingw32-g++")
(define vst "/home/tom/vstsdk2.4")

;; Instantiate module in an isolated namespace to produce C code.
;; The module should export `main' and `main-nsi' identifiers.
(define (stream-eval-module module [port (current-output-port)])
  (define c-code
    (eval
    `(begin
       (require "ai-array-c.rkt")
       (require ,module)
       (ai-array-c main #:nsi main-nsi))
    (make-base-namespace)))
  (display c-code port))

;; Convert module.rkt file module.g.h
(define (stream-gen-c infile outfile)
  (define outport (open-output-file outfile #:mode 'binary #:exists 'replace))
  (stream-eval-module `(file ,infile) outport)
  (close-output-port outport))

(define (system_ . as)
  (system
    (apply string-append
      (for/list ((a as)) (format "~a " a)))))

;; Combine vst template with .g.h to form executable .dll
(define (stream-gen-vst-dll cfile dllfile)
  (system_ "echo"
           g++
           "-g" "-Wall" "-Wno-unused-variable"  
           "-ffast-math"  "-O3" "-mfpmath=sse" "-msse2"
           "-I." "-I../copy"
           (format "-I~a" vst)
           (format "-I~a/public.sdk/source/vst2.x" vst)
           "-DPROC_FILE=\"synth.g.h\""
           "main_vst.cpp"
           "-luser32" "-lgdi32" "-lwsock32" "-shared"
           "-o" dllfile))


;; High level compile
(define (stream-build-vst basename)
  (define module-filename (format "~a.rkt" basename))
  (define dll-filename    (format "~a.dll" basename))
  (define gen-c-filename  (format "~a.g.h" basename))
  (stream-gen-c module-filename gen-c-filename)
  (stream-gen-vst-dll gen-c-filename dll-filename))



;; TEST
(stream-build-vst 'synth)



  
