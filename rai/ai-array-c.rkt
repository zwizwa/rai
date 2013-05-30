#lang racket/base
(require "tools.rkt"
         "type.rkt"
         "ai-array.rkt")
(provide (all-defined-out))

;; Generate C types and code from generic imperative block processing
;; program.

(define (ai-array/c expr
                    #:indextype (indextype 'int)
                    #:prefix (prefix "proc_"))
  (define (pfx x) (format "~a~a" prefix x))

  ;; FIXME: For unused variables, type is unknown.  Might just as well
  ;; not include them, i.e. expand as "//" to not define them, but
  ;; this requires the parameter macros to be adapted to not iterate
  ;; over undefined names.
  (define unknown "/*?*/ float")
  
  (define ctype
    (match-lambda
     ((struct type ('Float     #f '())) "float")
     ((struct type ('Int       #f '())) "int")
     ((struct type ('Undefined #f '())) unknown)
     ((struct type-var (_)) unknown) ;; FIXME: should not happen
     (type (error 'ctype (format "~a" type)))))
  
  (define *external-nodes* (make-hash))
  (define (node-name node) (caddr node))
  (define (node-dims node) (cadr node))
  (define (node-kind node) (length (node-dims node)))
  (define (register-nodes! ts/nss)
    (for ((t/ns ts/nss))
      (apply (lambda (t ns)
               (for ((n ns)) (hash-set! *external-nodes* (node-name n) t))
               ) t/ns)))

  ;; Connect node name to C function input references.
  (define (bind-node name)
    (let ((namespace (hash-ref *external-nodes* name false)))
      (if namespace
          ;; External node
          (format "~a->~a" namespace name)
          ;; Internal node
          (format "~a" name))))
  
  (define nest (make-parameter 0))
  (define (with-nest thunk)
    (parameterize ((nest (add1 (nest)))) (thunk)))
  (define tab-string
    "    "
    ;; "\t"
    )
  (define (tab1) (display tab-string))
  (define (tab) (for ((i (in-range (nest)))) (tab1)))

  (define (arrayref lst)
    (define (sqbr x) (format "[~a]" x))
    (apply string-append (map sqbr lst)))

  ;; Generate the structured state and param nodes.
  (define (gen-type tag nodes)
    (define stream
      (case tag
        ((in out) "* restrict ")
        (else "")))
    (printf "struct ~a {\n" (pfx tag))
    (for ((n nodes)
          (i (in-naturals)))
      (match n
        ((list base-type dims name)
         (tab1) (printf "~a ~a~a~a;\n"
                        (ctype base-type)
                        stream ;; t-indexed uses pointers
                        name
                        (arrayref (reverse dims))))))
    (printf "};\n")

    ;; Some C preprocessor annotation for the i/o nodes.
    (for ((n nodes))
      (printf "#define ~a_~a ~a\n"
              (pfx tag)
              (node-name n)
              (node-kind n)))
    (printf "#define ~a_~a(m) \\\n" (pfx "for") tag)
    (for ((n nodes))
      ;; (pp n)
      (printf "~am(~a) \\\n"
              tab-string
              (c-list `(,(node-name n)
                        ,(node-kind n)
                        ,(foldr * 1 (node-dims n))
                        ,@(reverse (node-dims n))))))
    (printf " \n")) ;; FIXME: space is a hack to make scribble output work
    

  (define (gen-types lst)
    (for ((t/n lst)) (apply gen-type t/n)))

  (define (ref expr)
    (match expr
      ((list-rest '@ var index)
       (format "~a~a"
               (bind-node var)
               (arrayref index)))
      (else
       (if (number? expr)
           (exact->inexact expr)
           expr))))
  
  (define (format-expr expr)
    ;; (pp expr)
    (match expr
      ((list-rest op args)
       (format "~a(~a)" op (c-list (map ref args))))))
  (define (line . a)
    (tab) (apply printf a))

  (define (state-pointer type/name index)
    (line "struct proc_~a * restrict ~a = (struct proc_~a *)(&state[(~a)&1]);\n"
          type/name type/name type/name
          index))


  (define (gen-statement expr)
    ;; (pp expr)
    (match expr
      ((list-rest 'block statements)
       (for ((s statements))
         (gen-statement s)))

      ((list 'p_loop (list i s e) expr)
       (begin
         (line "for (~a ~a = ~a; ~a < ~a; ~a ++) {\n" indextype i s i e i)
         (with-nest
          (lambda ()
            (when (eq? 't i)
              ;; Temporal loop swaps state buffers.
              (state-pointer "si" "t^0")
              (state-pointer "so" "t^1"))
            (gen-statement expr)
            ))
         (line "}\n")))

      ;; Local variable assignment (accumulator update)
;      ((list '()
;             (list-rest 'p_set dst src index))
;       (line "~a = ~a;\n" dst (ref src)))

      ;; Array assignment.
      ((list (list-rest '! name index) expr)
       (let ((indx (if (null? index) ""
                       (arrayref index))))
         (line "~a~a = ~a;\n"
               (bind-node name)
               indx (format-expr expr))))
      
      ;; Temp array declaration.
      ((list (list type name dims) _)
       (line "~a ~a~a;\n"
             (ctype type) name (arrayref dims)))

      ;; Local variable definition.
      ((list (list type name) expr)
       (line "~a ~a = ~a;\n"
             (ctype type) name
             (format-expr expr)))
      ))

  (define (gen-node-units units)
    ;; See prims.h for param/field order
    (define fields '(name unit min max scale curve))
    (define ((fld d) f)
      (let ((v (dict-ref d f)))
        (cond
         ((string? v) (format "~s" v))
         (else v))))
    (line "#define ~a(m) \\\n" (pfx "for_control"))
    (for (((node node-info) (in-dict units)))
      (with-nest
       (lambda ()
         (line "m(~a,~a) \\\n"
               node (c-list (map (fld node-info) fields))))))
    (line "\n"))
    
  (define (gen-code)
    (match expr
      ((list _
             meta
             io-nodes
             expr)
       ;; The state struct is represented by 2 isomorphic types: si
       ;; so, with different node names to allow ping-pong buffer
       ;; implementation.
       (let ((units (dict-ref meta 'units))
             (nodes (map list
                         '(si so in param out store)
                         io-nodes)))
         (register-nodes! nodes)
         (gen-types nodes)
         (gen-node-units units)
         
         (line "void ~a (\n" (pfx "loop"))
         (with-nest
          (lambda ()
            (let ((arg
                   (lambda (type name)
                     (line "struct ~a * restrict ~a,\n"
                           (pfx type) name))))
              (arg "si"    "state")
              (arg "in"    "in")
              (arg "param" "param")
              (arg "out"   "out")
              (arg "store" "store"))
            (line "~a t_endx) {\n\n" indextype)))
         (with-nest
          (lambda ()
            ;; Unrolled part needs si/so pointers defined.
            (state-pointer "si" "0")
            (state-pointer "so" "1")
            (gen-statement expr)))
         (printf "}\n")
         ))))

  ;; (stderr (gen-code))
  (with-output-to-string gen-code))


;; Compiler driver.
(define (ai-array-c program
                    #:tc (ignored #f)
                    #:nsi (nsi 0))   ;; Default: no inputs are streams
  (ai-array/c
   (ai-array program
             #:out-base Float
             #:nsi nsi)))
