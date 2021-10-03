#|
Code taken from bordeaux-threads-2
@author: sionescu@cddr.org
Atomic Reference added by: manfred@nnamgreb.de
MIT License
|#

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
   (java:jnew "java.util.concurrent.atomic.AtomicLong" value)))

(defconstant +atomic-long-cas+
  (java:jmethod "java.util.concurrent.atomic.AtomicLong" "compareAndSet"
                (java:jclass "long") (java:jclass "long")))

(defmethod atomic-cas ((int atomic-integer) old new)
  (declare (type %atomic-integer-value old new)
           (optimize (safety 0) (speed 3)))
  (java:jcall +atomic-long-cas+ (atomic-integer-cell int)
              old new))

(defconstant +atomic-long-incf+
  (java:jmethod "java.util.concurrent.atomic.AtomicLong" "getAndAdd"
                (java:jclass "long")))

(defconstant +atomic-long-get+
  (java:jmethod "java.util.concurrent.atomic.AtomicLong" "get"))

(defmethod atomic-get ((int atomic-integer))
  (declare (optimize (safety 0) (speed 3)))
  (java:jcall +atomic-long-get+ (atomic-integer-cell int)))

;; (defconstant +atomic-long-set+
;;   (java:jmethod "java.util.concurrent.atomic.AtomicLong" "set"
;;                 (java:jclass "long")))

;; (defun (setf atomic-integer-value) (newval atomic-integer)
;;   (declare (type atomic-integer atomic-integer)
;;            (type %atomic-integer-value newval)
;;            (optimize (safety 0) (speed 3)))
;;   (jcall +atomic-long-set+ (atomic-integer-cell atomic-integer)
;;          newval)
;;   newval)


;; atomic reference

(defstruct (atomic-reference
            (:constructor %make-atomic-reference (cell)))
  "Wrapper for java.util.concurrent.atomic.AtomicReference."
  cell)

(defmethod print-object ((ref atomic-reference) stream)
  (print-unreadable-object (ref stream :type t :identity t)
    (format stream "~S" (atomic-get ref))))

(defun make-atomic-reference (&key (value nil))
  (%make-atomic-reference
   (java:jnew "java.util.concurrent.atomic.AtomicReference" value)))

(defconstant +atomic-reference-cas+
  (java:jmethod "java.util.concurrent.atomic.AtomicReference" "compareAndSet"
           (java:jclass "java.lang.Object") (java:jclass "java.lang.Object")))

(defmethod atomic-cas ((ref atomic-reference) expected new)
  (declare (optimize (safety 0) (speed 3)))
  (java:jcall +atomic-reference-cas+ (atomic-reference-cell ref)
              expected new))

(defconstant +atomic-reference-get+
  (java:jmethod "java.util.concurrent.atomic.AtomicReference" "get"))

(defmethod atomic-get ((ref atomic-reference))
  (declare (optimize (safety 0) (speed 3)))
  (java:jcall +atomic-reference-get+ (atomic-reference-cell ref)))
