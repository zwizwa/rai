#lang racket/base
(require
 "tools.rkt"
 "stream-syntax.rkt" ;; base language core syntax
 "prim.rkt"          ;; For straight evaluation / partial eval.
 "peval.rkt"         ;; partial evaluation for prim.rkt functions
 "type.rkt")         ;; type unification

(provide ai-array
         ai-array/1
         ai-array/2
         with-ai-array-env)


;;; Compile to imperative program (i.e. C) operating on arrays.


;; PASS 1:
;;  - type annotation for variable nodes (base types and grid dimensions)
;;  - convert stream semantics to causal update semantics (feedback state alloc)
;;  - delay line allocation
;;  - phase annotation (block setup t=0 / block body t>0)
;;  - primitive partial evaluation
;;  - array index generation for explicit indexing (index permutation / generalized transpose)


;; PASS 2:
;;  - type-driven array index generation (for nodes not explicitly indexed in PASS 1)
;;  - low-level scalar type annotation (for C, LLVM)
;;  - stream input / scalar parameter input separation
;;  - i/o and state array dimension specification
;;  - generate imperative program with outer temporal loop and inner spatial loops

;; ( PASS 3: trivially convert to C: see ai-array-c.rkt )



;; Additional notes
;;
;; * This code is the result of a couple of months of incremental
;;   ad-hoc changes and needs cleanup.
;;
;; * Making evaluation lazy allows the implementation of dead code
;;   elimination.  The trouble here is to properly implement dynamic
;;   context.  Basically, dynamic parameters are not compatible with
;;   lazy eval.
;;
;; * Most of the work involves the creation of global context in a
;;   first pass, and the interpretation of this context in a second.
;;   All info is left in dictionaries implemented as dynamic
;;   parameters.  This also facilitates debugging.  If at any time it
;;   is nessary to provide a clean, pure functional interface, just
;;   fish the relevant data from the dynamic dictionaries.
;;
;; * The type annotation mechanism is a bit unorthodox.  It supports a
;;   mechanism of implicit indexing.  The reason for using implicit
;;   indexing is that it solves the behind-the-scenes state threading
;;   in a very elegant way.
;;
;;   Causal stream operators may appear inside spatial loops.  For
;;   each iteration, a separate "instance" of a causal stream operator
;;   needs to be allocated.  Shifting from the scalar to the grid view
;;   makes it trivial to solve this problem.
;;
;;   Abstracting such state management is a major feature of the
;;   language: it makes the language purely functional at the stream
;;   level, instead of object-oriented at the scalar level.
;;
;;   ( I accidentally bumped into this approach as originally making
;;   every operation auto-lifted seemed like a good idea.  It is not.
;;   Currently the loop contstruct is relatively orthodox, but
;;   internally the auto-lifting is still used to track grid
;;   indexing. )

(define debug #f)

(define (make-parameter* v)
  (if debug
      (make-parameter v)      ;; allow command-line experimentation
      (make-parameter 'ni)))  ;; Release: fail when local context is not provided.

;; Global bookkeeping
(define state-in  (make-parameter* '()))  ;; State i/o
(define state-out (make-parameter* '()))
(define state-0   (make-parameter* '()))  ;; si -> init map
(define store     (make-parameter* '()))  ;; Global array storage
(define in        (make-parameter* '()))
(define out       (make-parameter* '()))

(define time      (make-parameter* '()))  ;; Time block index, endx

(define bindings  (make-parameter* '()))  ;; Nested bindings

(define vbuf-in   (make-parameter* '()))  ;; Virtual buffer nodes
(define vbuf-out  (make-parameter* '()))
(define vbuf-attr (make-parameter* '()))

;; (define temp-dims (make-parameter* '()))  ;; Temp var dimensions.

;; Per node information.
(define names  (make-parameter '()))  ;; Original state node names from source.  Debug - Not unique!
(define phases (make-parameter '()))  ;; Code can be defined at t=0 t!=0 or any t.
(define units  (make-parameter '()))  ;; Contains unit / range / scale / pretty name info (e.g. for GUI)

(define (with-ai-array-env fn)
  (with-type-environment
   (lambda ()
     (parameterize ((bindings  '())
                    (state-in  '())
                    (state-out '())
                    (state-0   '())
                    (store     '())
                    (units     '())
                    (phases    '())
                    (names     '())
                    (vbuf-in   '())
                    (vbuf-out  '())
                    (vbuf-attr '())
;;                    (temp-dims '())
                    )
       (fn)))))



;; A permuted grid node.  Takes current loop indices and produces 2
;; values: the original grid node and its indices.
(define-struct p-node (tx n)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (lambda a (apply p-node-printer a)))])

(define (p-node-printer t port mode)
  (when mode (write-string "#<" port))
  (write-string (format "p-node:~a" (p-node-n t)) port)
  (when mode (write-string ">" port)))


;; Perform grid reference.
(define (index-node n indices)
  (if (p-node? n)
      ((p-node-tx n) indices)
      (values n indices)))

;; Generalized transpose.
(define (index-permute n permute)
  (make-p-node
   (lambda (indices)
     (let ((pi (permute indices)))
       (index-node n pi)))
   (if (p-node? n)
       (p-node-n n)
       n)))


;; "extended primitives" for pass1 output.
;; Currently not used.
(define p_index #f)
(define p_vbuf_update #f)
(define p_vbuf_read #f)
(define p_vbuf_attrib #f)
(define p_phase #f)
(define p_array #f)
(define p_decl_array #f)
(define p_set #f)


;; (define a0 'a0)
;; (define a1 (index-permute a0 reverse))
;; (define a2 (index-permute a1 reverse))
;; (index-node a0 '(i j))
;; (index-node a1 '(i j))
;; (index-node a2 '(i j))


;; Virtual buffers are a trick to integrate in-place update of arrays
;; into a functional language.  They represent a relation between 3
;; data nodes:
;;   - storage: real node, ends up in the imperative code
;;   - read:    temporary symbolic nodes abstracting storage read
;;   - write:   temporary vbuf objects abstracting storage write

(define-struct vbuf (storage read) #:transparent)


(define (ai-array/1 program
                    #:t_names [t_names '(t t_endx)]
                    #:out-base [out-base #f])
  
  ;; Delay lines get allocated into a single array with 2^N length for
  ;; easy modulo access using an AND mask.
  (define delay-nodes '())    ;; Delay line handles.
  (define delay-buf  #f)      ;; Delay storage node.
  (define delay-buf-mask #f)
  (define delay-index #f)     ;; Master delay memory index.
  (define delay-depth (make-hasheq))

  ;; Pass1 code traversal state.
  (define phase        (make-parameter #f))
  (define loop-index   (make-parameter #f))
  (define loop-type    (make-parameter #f))
  (define loop-size    (make-parameter #f))
  
  ;; Register a let-values binding between node and primitive operation.
  (define (save-binding n fn sym as) (save bindings `((,n) (,fn ,sym . ,(map dereference as)))))
  (define (save-multi-bindings bs e) (save bindings `(,bs ,e)))

  ;; Each node is associated to a definition phase: t=0, t>0 or any t,
  ;; represented by 0, 1 and #f.
  (define (make-var [gen (node-gensym)])
    (let* ((n (gen)))
      (typed-node! n)
      (save phases (cons n (phase)))
      n))
  (define (make-vars n) (for/list ((_ (in-range n))) (make-var)))

  ;; Dereference nodes in local loop context.
  (define (dereference n)
    (if (not (p-node? n))
        ;; indexing will be done in pass 2
        n
        ;; indexing is explicit, so do it here
        (let-values
            (((node indices) (index-node n (loop-index)))
             ((ref-node) (make-var)))
          (save-binding ref-node p_index 'p_index (cons node indices))
          (unify-expr! (list n ref-node))
          ref-node)))
  
  ;; Replace expression by binding.
  (define (expr->node/lit fn expr unify?)
    (match expr
      ((list-rest op as)
       (let* ((n (make-var)))
         (save-binding n fn op as)
         (when unify? (unify-expr! (cons n as)))
         n))
      (else expr))) ;; Already OK (varref or constant)

  (define (e-prim stx fn #:unify? [unify? #t])
    ;; Attempt expression simplification through partial evaluation
    ;; and tuck away any remaining expressions in the SSA dictionary.
    (lambda (sem . in-exprs)
      (let* ((expr (peval sem fn stx in-exprs))
             (node/lit (expr->node/lit fn expr unify?)))
        node/lit)))

  (define e-mul  (e-prim 'p_mul  p_mul))
  (define e-div  (e-prim 'p_div  p_div))
  (define e-add  (e-prim 'p_add  p_add))
  (define e-sub  (e-prim 'p_sub  p_sub))
  (define e-and  (e-prim 'p_and  p_and))
  (define e-or   (e-prim 'p_or   p_or))
  (define e-xor  (e-prim 'p_xor  p_xor))
  (define e-not  (e-prim 'p_not  p_not))
  (define e-quot (e-prim 'p_quot p_quot))
  (define e-mod  (e-prim 'p_mod  p_mod))
  (define e-sal  (e-prim 'p_sal  p_sal))
  (define e-sar  (e-prim 'p_sar  p_sar))
  

  
  (define (dl-index sem dl [offset #f])
    (let* ((delay-offset
            ;; Offset is obtained from vbuf read attributessem fn stx
            ((e-prim 'p_vbuf_attrib p_vbuf_attrib #:unify? #f) sem dl))
           (_ (unify! (node-type delay-offset) Int))
           
           (index
            (e-add sem delay-offset delay-index))
           ;; Optionally add another offset
           (index
            (if offset
                (e-add sem index offset)
                index))
           ;; Modulo access.  Delay buffer pool size is power of 2.
           (m-index (e-and sem index delay-buf-mask))
           )
      m-index))
    

  ;; Shift a value into a delay line (state output only).
  (define (dl-shift sem
                    dl-in
                    val)      ;; Value to write
    (let* ((dl-out
            ;; Connect input and output state node.
            (make-var (lambda () (make-vbuf delay-buf dl-in)))))
           
      ;; For later patching.
      (save vbuf-out dl-out)
      (save vbuf-in  dl-in)

      ;; For later delay line allocation.
      (save delay-nodes dl-out)
      (save-binding dl-out
                    p_vbuf_update
                    'p_vbuf_update
                    (list dl-in
                          (dl-index sem dl-out)
                          val))
      (unify-expr! (list dl-in dl-out))
      dl-out))

  ;; Read a value from a delay line (state input only).
  (define (dl-ref sem
                  delayline ;; Delay line abstraction 
                  index)    ;; Run time index
    (let* ((val (make-var))
           (m-index (dl-index sem delayline index)))
      ;; Note that `delayline' is an opaque vbuf read reference.  It
      ;; needs to be resolved by scanning the vbuf list `delay-nodes'.
      (save-binding val p_vbuf_read 'p_vbuf_read (list delayline m-index))

      ;; The max delay length is determined by the highest delay
      ;; offset.  At least one (dummy) literal reference is necessary.
      (when (number? index)
        (let ((depth (hash-ref delay-depth delayline (lambda _ 0))))
          (hash-set! delay-depth delayline (max depth index))))

      ;; Note that the length of a delay line is not part of its type.
      (unify-cross-rank! (:: Delay #f) val delayline)
      
      val))

  (define (index sem arr indx len)
    (let ((n (make-var)))
      (save-binding n p_index 'p_index (list arr indx))
      (unify-cross-rank! (:: Array len) n arr)
      n))

  (define (make-typed-var type [name #f])
    (let ((i-node(make-var)))
      (unify! (node-type i-node) type)
      i-node))

  (define (T-size T)
    (match (T (make-base-type 'Float)) ;; Dummy)
      ((struct type (_ index _)) index)))

  ;; Run a SISO system over input arrays
  (define (for/n sem
                 T            ;; traversable Array type
                 body         ;; foldable SISO body (s ... i ...) -> (s ... o ...)
                 accu-inits   ;; initial values
                 ins/a)       ;; stream inputs
    (define array-size (T-size T))
    (let*
        ;; Loop index, endx and accumulator nodes.
        ((index       (make-typed-var Int))
         (endx        array-size)
         (nb-accus    (length accu-inits))
         (nb-body-in  (length (ai-function-args body)))
         (nb-ins/a    (length ins/a))
         (nb-index    (- nb-body-in (+ nb-accus nb-ins/a)))
         (index-nodes (take (list index endx) nb-index))
         (accus       (make-vars nb-accus)))
      (let*-values
          (((accu-outs
             outs/a
             loop-bindings)
            (parameterize
                ((loop-index (cons index (loop-index)))
                 (loop-type (let ((t (loop-type)))
                              (lambda (base) (T (t base)))))
                 (bindings '()))
              (let*
                  ((T-index (list index))
                   (ins (map (node-load sem T T-index) ins/a))
                   (results
                    (values-list
                     (apply (ai-function-proc body)
                            sem
                            (append accus ins index-nodes))))
                   (accu-outs (take results nb-accus))
                   (outs      (drop results nb-accus))
                   (outs/a    (map (node-store sem T T-index) outs))
                   (loop-bindings (collect bindings)))
                (values accu-outs
                        outs/a
                        loop-bindings)))))
        ;; Accu in/out should have the same type.
        (for ((ao  accu-outs)
              (a   accus))
          (unify-expr! (list ao a)))

        ;; Loop output arrays and accumulator output scalars need to
        ;; be declared before the loop is entered.  This is a bit
        ;; awkward.  At this point we don't know yet if the node is an
        ;; output or not.  Patch it in PASS2.
        (for ((o outs/a))
          (save-binding o p_decl_array 'p_decl_array (list array-size)))

        
        (for ((a accus)
              (i accu-inits))
          (save-binding a p_copy 'p_copy (list i)))

        ;; Pack annotated loop info for next pass.
        (save-multi-bindings
         (append accus outs/a)
         `(p_for
           ,index
           ,endx
           ,array-size
           ,accus
           ,accu-outs
           ,loop-bindings))
        (apply values
               (append accus outs/a)))))
   
  ;; Why is cast/n a lazy operation?  What about e-cast here?
  ;(define (e-cast type)
  ;  (lambda (sem val)
  ;    (let ((b ((e-copy/cast sem) val)))
  ;      (unify! (node-type b) type)
  ;      b))

  (define (cast/n sem T in-node)
    (if (number? in-node)
        in-node
        (let ((casted-node (make-var)))
          ;; (unify! (node-type casted-node) T)
          ;; FIXME: How to unify only the base type?
          ;; Currently we just decouple unification here.
          (save-binding casted-node p_copy 'p_copy (list in-node))
          casted-node)))


  (define (get-time-nodes param-names)
    (take (time) (length param-names)))


  ;; Each node needs to be unique, but for some we have a pretty name,
  ;; so keep track of it.
  (define (make-named-nodes names/nb)
    (let* ((nb (if (list? names/nb) (length names/nb) names/nb))
           (nodes (make-vars nb)))
      (when (list? names/nb)
        (for ((n nodes)
              (p names/nb))
          (save names (cons n p))))
      nodes))

  (define (copy-node sem node)
    ((ai-function-proc ai-copy) sem node))
  (define (copy-nodes sem nodes)
    (map (lambda (n) (copy-node sem n)) nodes))
  (define (unique-node sem all-in)
    (lambda (node)
      (if (memq node all-in)
          (copy-node sem node) node)))


  ;; Tag nodes according to them being defined at t=0 (phase 0), t>0
  ;; (phase 1) or at any t (default, phase #f).

  ;; Copy across types, no unification.
  (define (e-copy/cast sem)
    (lambda (v)
      ((e-prim 'p_copy p_copy #:unify? #f) sem v)))
  
  (define (hold/n sem expr)
    (parameterize ((phase 0))
      (let ((vs (values-list ((ai-function-proc expr) sem))))
        (apply values
               (for/list ((v vs))
                 ((e-prim 'p_copy p_copy) sem v))))))
  
  (define (setup/n sem e0 e1)
    (define (tag e p)
      (values-list
       (parameterize ((phase p))
         ((ai-function-proc e) sem))))
    (let ((vs0 (tag e0 0))
          (vs1 (tag e1 1)))
      (apply values
             (for/list ((v0 vs0) (v1 vs1))
               ((e-prim 'p_phase p_phase) sem v0 v1)))))

  ;; Compile-time list supports: functions might return nodes of lists
  ;; as an unpacked array binding.
  (define (node-pack scalars)
    (if (not (list? scalars))
        scalars
        (let ((array (make-var))
              (size (length scalars)))
          (for ((s scalars))
            (unless (number? s)
              (unify-cross-rank! (:: Array size) s array)))
          (save-binding array p_array 'p_array scalars)
          array)))


  ;; Explicit pack/unpack for vector nodes.
  ;; If index is '() no indexing is necessary.
  (define (node-load sem T index)
    (lambda (arr)
      (if (null? index)
          arr
          (let ((el (make-var)))
            (save-binding el p_index 'p_index (cons arr (reverse index)))
            (unify-cross-rank! T out arr)
            el))))
  (define (node-store sem T index)
    (lambda (el-val)
      (if (null? index)
          el-val
          (let* ((arr (make-var))
                 (el  (make-var (lambda () `(! ,arr ,@(reverse index))))))
            ;; Note this is still a single-assingment.
            (save-binding el p_copy 'p_copy (list el-val))
            (unify-cross-rank! T el arr)
            (unify-expr! (list el el-val))
            arr))))
  
  
  (define (feedback/n sem state-names
                      in-nodes
                      time-names
                      update)
    (let*
        ((nb-state   (length state-names))
         (si-nodes/a (make-named-nodes state-names))  ;; -s- for setup
         (si-nodes   (map (node-load sem (loop-type) (loop-index)) si-nodes/a))
         (si-init    (map cdr state-names))
         (si-0       (map cons si-nodes/a si-init))
         
         (time-nodes (get-time-nodes time-names))
         (all-in     (append si-nodes in-nodes time-nodes))
         (all-out    (values-list (apply (ai-function-proc update) sem all-in)))
         
         (out        (drop all-out nb-state))
         
         (so-nodes   (take all-out nb-state))
         ;; Repack unpacked nodes.
         (so-nodes   (map node-pack so-nodes))

         ;; For registration, we use the array nodes.
         (so-nodes/a (map (node-store sem (loop-type) (loop-index)) so-nodes))

         ;; In and out need to be different nodes.
         (so-nodes/a (map (unique-node sem all-in) so-nodes/a)))

      
      ;; State in/out should have the same types.
      (for ((si si-nodes)
            (so so-nodes))
        (unify-expr! (list si so)))

      ;; (pp (list so-nodes so-nodes/a))

      ;; Keep track of the vector storage nodes.
      (nsave state-in    si-nodes/a)
      (nsave state-out   so-nodes/a)
      (nsave state-0     si-0)
      
      (apply values out)))

  

  (define (e-pow sem base exp)
    (if (integer? exp)
        (if (>= exp 0)
            (integer-power base exp (lambda (a b) (e-mul sem a b)) 1)
            (e-div sem 1 (e-pow sem base (* -1 exp))))
        ((e-prim 'p_pow p_pow) sem base exp)))

  (define (e-literal sem val) val)


  (define (e-prim-pred sym op)
    (lambda (sem . args)
      (let ((b (apply (e-prim sym op #:unify? #f) sem args)))
        (unify-expr! args)
        (unify! (node-type b) Int)  ;; Booleans are integer bitmasks
        b)))



  (define e-lt (e-prim-pred 'p_lt p_lt))
  
  (define (e-if sem c a b)
    (let ((r ((e-prim 'p_if p_if #:unify? #f) sem c a b)))
      (unify-expr! (list a b r))
      (unify! (node-type c) Int)  ;; Booleans are integer bitmasks
      r))
    

  (define (tag sem node record)
    ;; (pp record)
    (save units (cons node record))
    node)

  
  (define (_map sem fn arr-in)
    (let ((arr-out
           (lambda (i)
             (let ((el (arr-in i)))
               ;; cross-rank unify el and arr-in/arr-out
               (fn el)))))
      arr-out))
  
  (define semantics
    (make-ai #:feedback/n feedback/n
             #:for/n      for/n
             #:hold/n     hold/n
             #:setup/n    setup/n
             
             #:dl-shift   dl-shift
             #:dl-ref     dl-ref
             #:index      index
             
             #:literal    e-literal

             #:cast/n     cast/n
             #:copy       (e-prim 'p_copy  p_copy)
             
             #:add        e-add 
             #:sub        e-sub
             #:div        e-div
             #:mul        e-mul
             #:pow        e-pow

             #:and        e-and
             #:or         e-or
             #:xor        e-xor
             #:not        e-not
             #:mod        e-mod
             #:quot       e-quot

             #:sal        e-sal
             #:sar        e-sar

             #:exp        (e-prim 'p_exp   p_exp)
             #:log        (e-prim 'p_log   p_log)
             #:sin        (e-prim 'p_sin   p_sin)
             #:cos        (e-prim 'p_cos   p_cos)

             #:atan       (e-prim 'p_atan  p_atan)

             #:floor      (e-prim 'p_floor p_floor)
             
             #:lt         e-lt
             
             #:if         e-if

             #:debug      (e-prim 'p_debug p_debug)

             #:tag        tag

             #:_map       _map
             
             ))
  ;; Delays cycle in a single 2^n buffer.  Size is computed at the end
  ;; of the first pass, from the accumulateded `delay-allot' value.
  (define (setup-delay-nodes!)
    (set! delay-buf   (make-var))
    (save store       delay-buf)
    ;; The array size is required to be a power of 2, so we can do
    ;; wrap-around indexing using a bitmask.  Size is patched using
    ;; p_array_size primitive in pass 2 (not known during pass 1).
    (set! delay-buf-mask
          (e-sub semantics
                 ((e-prim 'p_array_size #f #:unify? #f) semantics delay-buf)
                 1))
    ;; Delay lines are indexed backwards in time, 1 -> T-1, 2 ->T-2,
    ;; so we run the main index backwrds.
    (set! delay-index (make-var))
    (save state-in   delay-index)
    (save state-out  (e-sub semantics delay-index 1))
    (unify! (node-type delay-index) Int)
    )


  ;; Run PASS1 to fill dictionaries.
  (parameterize
      ((loop-index '())
       (loop-type (lambda (base) base))
       (phase #f))
    (time (make-named-nodes t_names))
    ;; (for ((t-node (time))) (unify! (node-type t-node) Int))

    (setup-delay-nodes!)
          
    (let*
        ;; Create input nodes, keep names.
        ((in-nodes (map (lambda (s)
                          (make-var (lambda () s)))
                        (ai-function-args program)))
         
         ;; Feed input nodes to abstract program to obtain output
         ;; nodes.  Meanwhile the state-in/state-out and bindings
         ;; lists are being created.
         (out-nodes-intl
          (values-list
           (apply (ai-function-proc program)
                  semantics
                  in-nodes)))

         ;; Repack unpacked vectors
         (out-nodes-packed (map node-pack out-nodes-intl))
         
         ;; Make sure there is no direct path from in/state -> out,
         ;; and that out and state out nodes are different.
         
         (all-in (append (state-in) (state-out) in-nodes))
         (out-nodes (map (unique-node semantics all-in) out-nodes-packed))

         ;; This makes all output nodes unique.
         (out-nodes (copy-nodes semantics out-nodes-packed))
         )

      ;; Output base type constraint
      (when out-base
        (for ((o out-nodes))
          (let ((base (type-base (node-type o))))
            ;; (stderr-pp `(,out-base, base))
            (unify! base out-base))))

      ;; Save global I/O
      (in  in-nodes)
      (out out-nodes)
         
      ;; Clean up state i/o list: remove vbuf references and
      ;; reverse list.
      (state-in  (filter (not-in (vbuf-in))  (collect state-in)))
      (state-out (filter (not-in (vbuf-out)) (collect state-out)))
         
      ;; Finalize bindings by reversing top level.
      (bindings (collect bindings))

      (units (collect units))
         
      ;; Allocate delay lines.
      (let ((delay-buf-size
             (ceil-2^n  ;; Round up to the smallest power of two
              (for/fold
                  ((offset 0))
                  ((n delay-nodes))
                (save vbuf-attr (cons n offset))
                (let* ((read-node (vbuf-read n))
                       ;; (_ (pp `(,n ,read-node ,delay-depth)))
                       (depth
                        (hash-ref delay-depth read-node
                                  (lambda _
                                    (error 'delay-depth-unknown)))))
                  ;; (pp depth)
                  ;; Delay line size + 1 for write operation.
                  (+ offset depth 1))))))
         
        ;; All delays are stored in a single array.
        (unify! (node-type delay-buf)
                (make-type 'Array delay-buf-size
                           (list Float)))))

    ;; (pp (bindings))

    ))



(define (ai-array/2 nb-stream)

  ;; Map vbuf read references back to parent vbuf objects.
  (define vbuf-read->write
    (for/list ((v (vbuf-out)))
      (cons (vbuf-read v) v)))

  (define-values (v_t v_t_endx) (apply values (time)))

  (define phase  (make-parameter #f))

  (define (node-type-error n)
    (error 'node-type (format "~a" n)))
  (define (node-type n)
    (let*
        ((t (cond
             ((or
               (list? n)
               (symbol? n))
              (dict-ref (types) n (lambda _ (node-type-error n))))
             ((number? n) Float)  ;; FIXME: all constants default to float
             ;;((vbuf? n) (node-type (vbuf-storage n))) ;; FIXME: should't be called
             (else (node-type-error n)))))
      (if (type-var? t)
          ;; Type variable means type is not constrained enough.
          Undefined t)
      ;; t
      ))
  
  ;; Split inputs as stream (temporal) or constant parameter.
  ;; Later, maybe split name-based?
  (define in-stream (take (in) nb-stream))
  (define in-param  (drop (in) nb-stream)) 

  ;; External nodes use indexed access for get/set.
  (define external-node?
    (make-element-of
     (append (state-in) (in)
             (state-out) (out)
             (store))))
  ;; Temporal nodes use an extra time index.
  (define temporal-node?
    (make-element-of
     (append in-stream
             (out))))

  ;; Keep track of array indexing variables when entering loop context.
  (define (loop-context-indices lc) (reverse lc))
  (define (loop-context-enter i lc) (cons i lc))
  (define (loop-context-inner lc) (car lc))


  ;; Only insert bindings when they are in the correct phase.
  (define (in-phase? vars)
    (define p (phase))
    (define (gen? v)
      (let ((v_p (dict-ref (phases) v)))
        (or (not v_p)     ;; no restriction: defined in all phases
            (= v_p p))))  ;; only correct phase
    (or (not p)           ;; code does not have phases -> always in-phase
        (andmap gen? vars)))
  

  ;; The `annotate-def' and `annotate-ref' functions will provide the
  ;; array indexing for each node.
  ;;
  ;; External nodes are always indexed by reference, either scalar or
  ;; array.  Temporal i/o nodes have an extra time parameter.  Note
  ;; that this needs to happen in pass 2 since the `external-node?'
  ;; and `temporal-node?' info is not available during pass 1.

  (define (time-coords v)
    (if (temporal-node? v) (list 't) '()))

  (define (node-base-type v) (type-base (node-type v)))
  
  (define (annotate-def lc)
    (lambda (v)
      (match v
        ((list-rest '! v index)
         `(! ,v ,@index ,@(time-coords v)))
        (v
         (if (external-node? v)
             `(! ,v ,@(time-coords v))
             `(,(node-base-type v) ,v))))))
  
  (define (annotate-ref lc)
    (lambda (v)
      (match v
        ((list-rest '@ v index)
         `(@ ,v ,@index ,@(time-coords v)))
        (v
         (if (external-node? v)
             `(@ ,v ,@(time-coords v))
             v)))))
  
  (define (annotate-statement lc)
    (match-lambda
     ((list var (list-rest op args))
      `(,((annotate-def lc) var)
        (,op ,@(map (annotate-ref lc) args))))))

  
            


  ;; Main annotation routine for traversing the PASS1 binding tree and
  ;; associated node annotation dictionaries.
  
  ;; This removes virtual primitives from the bindings dictionary and
  ;; builds the output imperative program with `block' and `loop'
  ;; structures, fully type annotated internal scalar nodes, `@' and
  ;; `!' array ref/store operations.

  ;; p_vbuf_update:  resolve vbuf reference for storage write
  ;; p_vbuf_read:    resolve vbuf reference for storage read
  ;; p_vbuf_attrib:  resolve vbuffer attribute (e.g. delay offset)
  ;; p_array_size:   resolve size of an array node
  ;; p_index:        use explicit indexing to override implicit indexing
  ;; p_phase:        phase-conditional code
  ;; p_decl_array:   declare a temporary array
  
  (define (annotate-binding lc)
    (lambda (binding)
      ;; (pp binding)
      (match binding
        ((list vars expr)
         (if (not (in-phase? vars))
             '()
             (match binding
               ((list _
                      (list 'p_for
                            index
                            endx
                            array-size
                            accus
                            accu-outs
                            loop-bindings))
                (let ((lc-loop (loop-context-enter index lc)))
                  `((p_loop
                     (,index 0 ,array-size)
                     (block
                      ,@(annotate-bindings lc-loop loop-bindings)
                      ;; Patch loop accu output.
                      ,@(for/list ((i accus)
                                   (o accu-outs))
                          `(,((annotate-def lc-loop) `(! ,i))
                            (p_copy ,((annotate-ref lc-loop) o))))
                      )))))
               
               ((list (list array)
                      (list-rest _ 'p_array els))
                (for/list ((el els)
                           (i (in-naturals)))
                  `((! ,array ,i) (p_copy ,((annotate-ref lc) el)))))
               ((list (list vbuf)
                      (list _ 'p_vbuf_update
                            vbuf-read-ref ;; ignored, all info is in vbuf
                            index value))
                ;; In-place updates are wrapped in virtual types.
                `(((! ,(vbuf-storage vbuf) ,index)
                   (p_copy ,((annotate-ref lc) value)))))

               ((list (list array)
                      (list-rest _ 'p_decl_array  dims))
                (if (external-node? array)
                    '()
                    `(((,(node-base-type array) ,array ,dims) '()))))

               ;; Deep copy.
               ;; FIXME: how to avoid dependency on loop context?
               ((list (list out)
                      (list _ 'p_copy in))
                (let ((lci (loop-context-indices lc))
                      (til (type-index-list (node-type out)))
                      (s   `(,out (p_copy ,in))))
                  ;; (pp binding)
                  ;; (pp (list lci til))
                  (if (< (length lci) (length til))
                      ;; FIXME: generating index nodes here.  Is that ok?
                      (let* ((i (string->symbol
                                 (format "i~a" (length lci))))
                             (lc-loop (loop-context-enter i lc)))
                        `((p_loop 
                           (,i 0 ,(car til))
                           ,((annotate-statement lc-loop) s))))
                      `(,((annotate-statement lc) s))
                      )))

               ;; Side effect store
               ((list '()
                      (list _ 'p_set (list-rest dst indices) r))
                `(((! ,dst ,@indices) (p_copy ,((annotate-ref lc) r)))))
               
               
               ;; All other forms only bind one variable.
               ((list (list v) expr)
                `(,((annotate-statement lc)
                    `(,v ,(match expr
                            ((list-rest _ 'p_index a is)
                             `(p_copy (@ ,a . ,is)))
                            ((list _ 'p_array_size arr)
                             `(p_copy ,(car (type-index-list (node-type arr)))))
                                             
                            ((list _ 'p_vbuf_attrib v)
                             (let ((vbuf (if (vbuf? v) v
                                             (dict-ref vbuf-read->write v))))
                               `(p_copy ,(dict-ref (vbuf-attr) vbuf))))
                            ((list _ 'p_vbuf_read  vbuf-read-ref index)
                             (let* ((v (dict-ref vbuf-read->write
                                                 vbuf-read-ref))
                                    (arr (vbuf-storage v)))
                               `(p_copy  (@ ,arr ,index))))
                            ((list _ 'p_phase v0 v1)
                             `(p_copy ,(case (phase) 
                                         ((0) v0)
                                         ((1) v1))))
                            ((list-rest _ op args)
                             `(,op . ,args)))))))
               ))))))
        
  ;; Wrap annotate-binding for splicing into parent context.
  (define (annotate-bindings lc ns)
    (apply append (map (annotate-binding lc) ns)))
  


  


  ;; Data type annotation (e.g. for generating C struct/array defs).
  (define (data nodes)
    (for/list ((n nodes))
      (let* ((t (node-type n))
             (til (type-index-list t)))
        ;; (pp (list t til))
        `(,(type-base t) ,til ,n))))

  ;; Expand the bindings structured from pass1, binding it to a local
  ;; time coordinate `t'.
  (define (annotate-time-bindings bs #:phase [p #f])
    (parameterize ((phase p))
      `(,@(map (annotate-statement '())
               `((,v_t      (p_copy t))
                 (,v_t_endx (p_copy t_endx))))
        ,@(annotate-bindings '() bs))))


  ;; If all phases are don't-care (#f) then we don't need to generate
  ;; a loop preamble.
  (define have-phases (ormap cdr (phases)))
  ;; (pp (phases))
  
  ;; Convert pass1 dictionar to imperative block form.
  (define compiled-expr
    `(block-function
      
      ((units . ,(units))    ;; GUI annotation info.
       (inits . ,(state-0))) ;; State initial values
      ;; Numerical I/O
      (,(data (state-in))
       ,(data (state-out))
       ,(data in-stream)
       ,(data in-param)
       ,(data (out))
       ,(data (store)))
      ,(if have-phases
           `(block
             ((,Int t) (p_copy 0))
             ;; First sample of block, contains per block setup code.
             ,@(annotate-time-bindings (bindings) #:phase 0)
             ;; Main time loop.
             (p_loop (t 1 t_endx)
                     (block
                      ,@(annotate-time-bindings (bindings) #:phase 1))))
           ;; Better to generate simpler code if there are no phases.
           `(p_loop (t 0 t_endx)
                    (block
                     ,@(annotate-time-bindings (bindings) #:phase #f))))))
  
  ;; (pp compiled-expr)
  compiled-expr)


                      
;; Produce abstract imperative mostly-SSA program.
;; This can be passed to a language back-end code generator.
(define (ai-array program
                  #:out-base (out-base #f)
                  #:nsi (nsi #f))
  (with-ai-array-env
   (lambda ()
     ;; Build binding tree and node annotation dictionaries.
     (ai-array/1 program #:out-base out-base)
     ;; Returns pure code object representing imperative program.
     (let ((nb-stream (if nsi nsi (length (in)))))
       (ai-array/2 nb-stream)))))



