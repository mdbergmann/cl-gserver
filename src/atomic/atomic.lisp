(in-package :cl-gserver.atomic)

;; ---------- reference -------------

(defvar *the-ref* nil "special var used for CAS")

(defstruct (atomic-reference
            (:constructor %make-atomic-reference (cell)))
  "Wrapper atomics package."
  cell)

(defun make-atomic-reference (&key (value nil))
  (%make-atomic-reference value))

(defmethod print-object ((ref atomic-reference) stream)
  (print-unreadable-object (ref stream :type t :identity t)
    (format stream "~S" (atomic-get ref))))

(defmethod atomic-cas ((ref atomic-reference) expected new)
  (let* ((*the-ref* (slot-value ref 'cell))
         (cas-result (atomics:cas *the-ref* expected new)))
    (when cas-result
      (setf (slot-value ref 'cell) *the-ref*))
    cas-result))

(defmethod atomic-get ((ref atomic-reference))
  (slot-value ref 'cell))

;; --------------- integer/long --------------

(defvar *the-int* nil "special var used for CAS")

(defstruct (atomic-integer
            (:constructor %make-atomic-integer (cell)))
  "Wrapper atomics package."
  cell)

(defun make-atomic-integer (&key (value 0))
  (%make-atomic-integer value))

(defmethod print-object ((int atomic-integer) stream)
  (print-unreadable-object (int stream :type t :identity t)
    (format stream "~S" (atomic-get int))))

(defmethod atomic-get ((int atomic-integer))
  (slot-value int 'cell))

(defmethod atomic-cas ((int atomic-integer) expected new)
  (let* ((*the-int* (slot-value int 'cell))
         (cas-result (atomics:cas *the-int* expected new)))
    (when cas-result
      (setf (slot-value int 'cell) *the-int*))
    cas-result))
