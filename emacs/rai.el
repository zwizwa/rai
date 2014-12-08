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

(defvar *pd-send-process* 
  (pd-make-client "localhost" 12345))

(defun pd-send (message)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert message)
    (process-send-string *pd-send-process* (buffer-string))))

(defun pd-gather-nums ()
  (let* ((cmds '())
         (str (thing-at-point 'sentence))
         (expr (read str)))
    (labels ((gather (it)
               (cond 
                ((listp it)
                 (mapc #'gather it))
                ((numberp it) 
                 (let ((cmd (format "p%d %f;\n" (length cmds) it)))
                   (push cmd cmds))))))
      (gather expr)
      (reverse cmds))))

(defun pd-send-nums ()
  (interactive)
  ;; (pd-send (apply #'concat (pd-gather-nums)))
  (mapc #'pd-send (pd-gather-nums)))
