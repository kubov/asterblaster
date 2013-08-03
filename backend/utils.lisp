(in-package :asterblaster)

(defun assoc-cdr (&rest args)
  (cdr (apply #'assoc args)))

(defun forever (fun)
  (lambda () (loop do (funcall fun))))
