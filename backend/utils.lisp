(in-package :asterblaster)

(defun assoc-cdr (&rest args)
  (cdr (apply #'assoc args)))
