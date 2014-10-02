#lang scheme/base
(require scheme/file
         scheme/system)

;; ARM Cortex-M4 Assembler

(define (op3 op) (lambda (d a b) (format "\t~a ~a, ~a, ~a\n" op d a b)))
(define qadd (op3 "qadd"))

(define (tmpfile ext)
  (path->string
   (make-temporary-file
    (string-append "arm-dsp-~a." ext))))

(define prefix (make-parameter "arm-none-eabi-"))
(define cflags (make-parameter "-g -fPIC -mcpu=cortex-m4 -mthumb -mfloat-abi=hard"))
(define (tool fmt . args)
  (system (apply format (string-append (prefix) fmt) args)))

(define (run-assembler code #:prefix [prefix "arm-none-eabi-"])
  (let* ((asm (tmpfile "S"))
         (obj (tmpfile "o"))
         (bin (tmpfile "bin")))
    (with-output-to-file asm
      (lambda () (display code))
      #:exists 'truncate)
    (tool "gcc ~a -c ~a -o ~a\n" (cflags) asm obj)
    (tool "objcopy ~a -Obinary ~a" obj bin)
    ;; (tool "objdump -d ~a"  obj)
    ;; (system (format "hd ~a" bin))
    (let ((rv (file->bytes bin)))
      (for ((f (list asm obj bin))) (delete-file f))
      rv)))

(define-syntax-rule (define-registers regs)
  (define-values regs (apply values 'regs)))

(define-registers (r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 sp lr pc))

(define (assembler . args)
  (run-assembler (apply string-append args)))

    
(define (test)
  ;(start-gdb)
  ;(connect)
  ;; (mi> "-target-select remote :3333")
  (assembler
   (qadd r1 r2 r3)
   (qadd r2 r1 r3)))

     
(test)

    
