(defpackage :cl-gserver.utils
  (:nicknames :utils)
  (:use :cl)
  (:export #:mkstr))

(in-package :cl-gserver.utils)

(defun mkstr (&rest args)
  "Converts all parameters to string and concatenates them."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
