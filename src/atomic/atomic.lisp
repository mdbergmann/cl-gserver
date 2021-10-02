(defpackage :cl-gserver.atomic
  (:use :cl)
  (:nicknames :atomic)
  (:export #:make-atomic-reference
           #:atomic-reference-cas
           #:atomic-reference-value))

(in-package :cl-gserver.atomic)

(defvar *the-ref* nil "special var used for CAS")

(defstruct (atomic-reference
            (:constructor %make-atomic-reference (cell)))
  "Wrapper atomics package."
  cell)

(defmethod print-object ((ar atomic-reference) stream)
  (print-unreadable-object (ar stream :type t :identity t)
    (format stream "~S" (atomic-reference-value ar))))

(defun make-atomic-reference (&key (value nil))
  (%make-atomic-reference value))

(defun atomic-reference-cas (atomic-reference expected new)
  (let* ((*the-ref* (slot-value atomic-reference 'cell))
         (cas-result (atomics:cas *the-ref* expected new)))
    (when cas-result
      (setf (slot-value atomic-reference 'cell) *the-ref*))
    cas-result))

(defun atomic-reference-value (atomic-reference)
  (slot-value atomic-reference 'cell))
