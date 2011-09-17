; This is a comment

(defun simple-function (lst)
  "Run a simple function."
  (cond ((null lst) 0)
	(t (+ 1 (cdr lst))) ))

(setq hook 'simple-function)
