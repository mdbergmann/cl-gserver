#|
Code taken from bordeaux-threads-2
@author: sionescu@cddr.org
Atomic Reference added by: manfred@nnamgreb.de
MIT License
|#

(defpackage :cl-gserver.atomic
  (:use :cl)
  (:nicknames :atomic)
  (:import-from :java #:jnew #:jcall #:jclass #:jmethod)
  (:export #:make-atomic-integer
           #:atomic-integer-cas
           #:atomic-integer-value
           #:make-atomic-reference
           #:atomic-reference-cas
           #:atomic-reference-value))

(in-package :cl-gserver.atomic)

(defstruct (atomic-integer
            (:constructor %make-atomic-integer (cell)))
  "Wrapper for java.util.concurrent.atomic.AtomicLong."
  cell)

(defmethod print-object ((aint atomic-integer) stream)
  (print-unreadable-object (aint stream :type t :identity t)
    (format stream "~S" (atomic-integer-value aint))))

(deftype %atomic-integer-value ()
  '(unsigned-byte 63))

(defun make-atomic-integer (&key (value 0))
  (check-type value %atomic-integer-value)
  (%make-atomic-integer
   (jnew "java.util.concurrent.atomic.AtomicLong" value)))

(defconstant +atomic-long-cas+
  (jmethod "java.util.concurrent.atomic.AtomicLong" "compareAndSet"
           (jclass "long") (jclass "long")))

(defun atomic-integer-cas (atomic-integer old new)
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value old new)
           (optimize (safety 0) (speed 3)))
  (jcall +atomic-long-cas+ (atomic-integer-cell atomic-integer)
         old new))

(defconstant +atomic-long-incf+
  (jmethod "java.util.concurrent.atomic.AtomicLong" "getAndAdd"
           (jclass "long")))

(defun atomic-integer-decf (atomic-integer &optional (delta 1))
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value delta)
           (optimize (safety 0) (speed 3)))
  (let ((increment (- delta)))
    (+ (jcall +atomic-long-incf+ (atomic-integer-cell atomic-integer)
              increment)
       increment)))

(defun atomic-integer-incf (atomic-integer &optional (delta 1))
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value delta)
           (optimize (safety 0) (speed 3)))
  (+ (jcall +atomic-long-incf+ (atomic-integer-cell atomic-integer)
            delta)
     delta))

(defconstant +atomic-long-get+
  (jmethod "java.util.concurrent.atomic.AtomicLong" "get"))

(defun atomic-integer-value (atomic-integer)
  (declare (type atomic-integer atomic-integer)
           (optimize (safety 0) (speed 3)))
  (jcall +atomic-long-get+ (atomic-integer-cell atomic-integer)))

(defconstant +atomic-long-set+
  (jmethod "java.util.concurrent.atomic.AtomicLong" "set"
           (jclass "long")))

(defun (setf atomic-integer-value) (newval atomic-integer)
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value newval)
           (optimize (safety 0) (speed 3)))
  (jcall +atomic-long-set+ (atomic-integer-cell atomic-integer)
         newval)
  newval)


;; atomic reference

(defstruct (atomic-reference
            (:constructor %make-atomic-reference (cell)))
  "Wrapper for java.util.concurrent.atomic.AtomicReference."
  cell)

(defmethod print-object ((ar atomic-reference) stream)
  (print-unreadable-object (ar stream :type t :identity t)
    (format stream "~S" (atomic-reference-value ar))))

(defun make-atomic-reference (&key (value nil))
  (%make-atomic-reference
   (jnew "java.util.concurrent.atomic.AtomicReference" value)))

(defconstant +atomic-reference-cas+
  (jmethod "java.util.concurrent.atomic.AtomicReference" "compareAndSet"
           (jclass "java.lang.Object") (jclass "java.lang.Object")))

(defun atomic-reference-cas (atomic-reference expected new)
  (declare (optimize (safety 0) (speed 3)))
  (jcall +atomic-reference-cas+ (atomic-reference-cell atomic-reference)
         expected new))

(defconstant +atomic-reference-get+
  (jmethod "java.util.concurrent.atomic.AtomicReference" "get"))

(defun atomic-reference-value (atomic-reference)
  (declare (optimize (safety 0) (speed 3)))
  (jcall +atomic-reference-get+ (atomic-reference-cell atomic-reference)))
