(in-package :sento.atomic)

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

(defmethod atomic-cas ((ref atomic-reference) old new)
  "Synonym for COMPARE-AND-SWAP.
Atomically store NEW in the cell slot of REF if OLD is eq to the current value of cell slot.
Return non-NIL if this atomic operaion succeeded, or return NIL if it failed."
  (atomics:cas (atomic-place ref) old new))

(defmethod atomic-get ((ref atomic-reference))
  (atomic-place ref))

(defmethod atomic-swap ((ref atomic-reference) fn &rest args)
  "Updates the cell value of REF atomically to the value returned by calling function
FN with ARGS and the previous cell value of REF. The first argument of FN should be
the previous cell value of REF."
  (loop :for old := (atomic-get ref)
        :for new := (apply fn old args)
        :until (or (eq :end new) (atomic-cas ref old new))
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

(defmethod atomic-swap ((int atomic-integer) fn &rest args)
  (loop :for old = (atomic-get int)
        :for new = (apply fn old args)
        :until (atomic-cas int old new)
        :finally (return new)))
