
;; Quick start:

;; Load this file.  It will bind M-up and M-down to logarithmically
;; inc/dec the number at point, followed by sending in pd format all
;; numbers marked with quote (') indexed in traversal order.

;; Then use the form `lambda/params' instead of `lambda' to generate a
;; processor with parameters for each quoted number.  Compile to
;; something that accepts pd messages.

;; see rai/test/doodle.rkt


;; logarithmic number editing
(require 'thingatpt)
(defun rai-m-up ()
  (interactive)
  (rai-scale-number-at-point 1.1)
  (pd-send-nums))
(defun rai-m-down ()
  (interactive)
  (rai-scale-number-at-point 0.91)
  (pd-send-nums))

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

(defun pd-make-client (host port)
  (make-network-process
   :name "pd-client"
   :coding 'binary
   :host host
   :service port
   :type 'datagram))

(defvar *pd-send-process* 
  (pd-make-client "127.0.0.1" 12345))

;; (setq *pd-send-process* (pd-make-client "127.0.0.1" 12345))

(defun pd-send (msg)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert msg)
    ;; (message msg)
    (process-send-string *pd-send-process* (buffer-string))))

(defun pd-gather-nums ()
  (let* ((cmds '())
         (str (thing-at-point 'defun));;'sentence
         (expr (read str)))
    ;;(message str)
    (labels ((gather (it)
               (when (listp it)
                 (if (and (= (length it) 2)
                          (eq (car it) 'quote)
                          (numberp (cadr it)))
                     (let ((cmd (format "p%d %f;\n" (length cmds) (cadr it))))
                       (push cmd cmds))
                   (mapc #'gather it)))))
      (gather expr)
      (reverse cmds))))

(defun pd-send-nums ()
  (interactive)
  ;; (pd-send (apply #'concat (pd-gather-nums)))
  (mapc #'pd-send (pd-gather-nums)))
