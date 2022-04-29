(in-package :cl-gserver.atomic)

;; ---------- reference -------------

(defvar *the-ref* nil "special var used for CAS")

(defstruct (atomic-reference
            (:constructor %make-atomic-reference (cell)))
  "Wrapper atomics package."
  cell)

(defun make-atomic-reference (&key (value nil))
  #+ccl (%make-atomic-reference (make-array 1 :initial-element value))
  #-ccl (%make-atomic-reference (cons value nil)))

(defmethod print-object ((ref atomic-reference) stream)
  (print-unreadable-object (ref stream :type t :identity t)
    (format stream "~S" (atomic-get ref))))

(defmacro atomic-place (ref)
  #+ccl
  `(svref (slot-value ,ref 'cell) 0)
  #-ccl
  `(car (slot-value ,ref 'cell)))

(defmethod atomic-cas ((ref atomic-reference) expected new)
  (atomics:cas (atomic-place ref) expected new))

(defmethod atomic-get ((ref atomic-reference))
  (atomic-place ref))

(defmethod atomic-setf ((ref atomic-reference) new)
  (loop :for old = (atomic-get ref)
        :until (atomics:cas (atomic-place ref) old new)
        :finally (return new)))

;; --------------- integer/long --------------

(defvar *the-int* nil "special var used for CAS")

(defstruct (atomic-integer
            (:constructor %make-atomic-integer (cell)))
  "Wrapper atomics package."
  cell)

(defun make-atomic-integer (&key (value 0))
  #+ccl (%make-atomic-reference (make-array 1 :initial-element value))
  #-ccl (%make-atomic-reference (cons value nil)))

(defmethod print-object ((int atomic-integer) stream)
  (print-unreadable-object (int stream :type t :identity t)
    (format stream "~S" (atomic-get int))))

(defmethod atomic-get ((int atomic-integer))
  (atomic-place int))

(defmethod atomic-cas ((int atomic-integer) expected new)
  (atomics:cas (atomic-place int) expected new))

(defmethod atomic-setf ((int atomic-integer) new)
  (loop :for old = (atomic-get int)
        :until (atomics:cas (atomic-place int) old new)
        :finally (return new)))
