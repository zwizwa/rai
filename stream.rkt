#lang racket/base
;; Wrapper for stream-syntax.rkt implementing a Racket #lang
(require
 "stream-syntax.rkt"
 "stream-meta.rkt"
 "type.rkt")
(provide
 ;; Racket basic syntax
 #%module-begin
 #%top
 #%top-interaction
 #%datum

 planet
 for-syntax
 
 ;; Scheme forms
 require provide all-defined-out
 ;;define
 define-syntax-rule define-syntax
 (for-syntax syntax-rules)
 let  let-values
 let* let*-values
 quote quasiquote unquote

 ;; Types
 Int Float

 (all-from-out "stream-syntax.rkt")
 (all-from-out "stream-meta.rkt")
 (rename-out

  (ai-define          define)
  (ai-tag             tag)

  ;; Language stuff
  ;; (ai-datum           #%datum)  ;; FIXME: Insert proper ai-literal wrapper for data.
  (ai-app             #%app)
  (ai-lambda          lambda)
  (ai-values          values)

  
  ;; * and + are multi-arg.
  (ai-prod  *)
  (ai-sum   +)
  (ai-div   /)
  (ai-sub   -)
  (ai-pow   ^)
  (ai-exp   exp)
  (ai-sin   sin)
  (ai-cos   cos)
  (ai-log   log)
  (ai-atan  atan)
  (ai-floor floor)

  (ai-and   and)
  (ai-or    or)
  (ai-not   not)
  (ai-xor   xor)
  
  (ai-lt    <)
  
  (ai-if    if)
  
  (ai-setup setup)
  (ai-hold  hold)

  (ai-vector vector)
  (ai-matrix matrix)
  (ai-map    map)
  (ai-length length)
  
  (ai-debug debug)
  
  (ai-dl-shift dl-shift)
  (ai-dl-ref   dl-ref)
  (ai-index index)
  

  (ai-loop  loop)
  
  ))

         
