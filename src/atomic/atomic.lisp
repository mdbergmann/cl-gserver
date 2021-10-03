(defpackage :cl-gserver.atomic
  (:use :cl)
  (:nicknames :atomic)
  (:export #:make-atomic-reference
           #:atomic-cas
           #:atomic-get))

(in-package :cl-gserver.atomic)

;; ---------- reference -------------

(defvar *the-ref* nil "special var used for CAS")

(defstruct (atomic-reference
            (:constructor %make-atomic-reference (cell)))
  "Wrapper atomics package."
  cell)

(defmethod print-object ((ar atomic-reference) stream)
  (print-unreadable-object (ar stream :type t :identity t)
    (format stream "~S" (atomic-get ar))))

(defun make-atomic-reference (&key (value nil))
  (%make-atomic-reference value))

(defun atomic-cas (atomic-reference expected new)
  (let* ((*the-ref* (slot-value atomic-reference 'cell))
         (cas-result (atomics:cas *the-ref* expected new)))
    (when cas-result
      (setf (slot-value atomic-reference 'cell) *the-ref*))
    cas-result))

(defun atomic-get (atomic-reference)
  (slot-value atomic-reference 'cell))

