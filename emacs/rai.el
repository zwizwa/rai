
;; Quick start:

;; Load this file.  It will bind M-up and M-down to logarithmically
;; inc/dec the number at point, followed by sending in pd format all
;; numbers marked with quote (') indexed in traversal order.

;; Messages can be seen e.g. with "netcat -lup 12345"

;; Then use the form `lambda/params' instead of `lambda' to generate a
;; processor with parameters for each quoted number.  Compile to
;; something that accepts pd messages.

;; Now check rai/test/doodle.rkt at the bottom.

;; logarithmic number editing
(require 'thingatpt)
(defun rai-m-up ()
  (interactive)
  (rai-scale-number-at-point 1.1)
  (rai-send-nums))
(defun rai-m-down ()
  (interactive)
  (rai-scale-number-at-point 0.91)
  (rai-send-nums))

(defun rai-scale-number-at-point (factor)
  (let*
      ((word (thing-at-point 'symbol))
       (num (string-to-number word)))
    (when (not (zerop num)) ;; Only operate on numbers
      (let* 
          ((updated (* num factor))
           ;; Round digits through exponential notation.
           (snum (format "%.1e" updated))
           ;; But keep ordinary number format
           (snum (number-to-string (string-to-number snum)))
           (bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (stop  (cdr bounds)))
        (delete-region start stop)
        (insert snum)))))

(global-set-key (kbd "<M-up>")   'rai-m-up)
(global-set-key (kbd "<M-down>") 'rai-m-down)


;; RAI

;; https://github.com/mlang/emacs-lisp/blob/master/osc.el

;; Gather quoted numbers and assign structural addresses.

(defun rai-gather-nums-at-point ()
  (let* ((str (thing-at-point 'defun))
         (expr (read str)))
    (message str)
    (rai-gather-nums expr)))

(defun rai-gather-nums (expr)
  (let* ((cmds '())
         (node 0))

    ;; Strip to the inner form of lambda/params (see stream-lib.rkt)
    (setq expr (if (eq (car expr) 'define-values) (caddr expr) '()))
    (setq expr (if (eq (car expr) 'lambda/params) (caddr expr) '()))
    
    (cl-labels ((gather (it)
	       (setq node (+ 1 node))
               (when (listp it)
                 (if (and (= (length it) 2)
                          (eq (car it) 'quote)
                          (numberp (cadr it)))
                     (let ((cmd (list node (cadr it))))
                       (push cmd cmds))
                   (mapc #'gather it)))))
      (gather expr)
      (reverse cmds))))




(defun rai-make-client (host port)
  (make-network-process
   :name "rai-client"
   :coding 'binary
   :host host
   :service port
   :type 'datagram))

(defvar *rai-send-process* 
  (rai-make-client "127.0.0.1" 12345))

;; (setq *rai-send-process* (rai-make-client "127.0.0.1" 12345))

(defun rai-send-format (node number)
  (format "p%d %f;\n" node number)) ;; pd netsend

(defun rai-send (msg)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (apply 'rai-send-format msg))
    ;; (message msg)
    (process-send-string *rai-send-process* (buffer-string))))


(defun rai-send-nums ()
  (interactive)
  ;; (rai-send (apply #'concat (rai-gather-nums)))
  (mapc #'rai-send (rai-gather-nums-at-point)))


;; TODO: make M-left, M-right cycle through the parameters.
