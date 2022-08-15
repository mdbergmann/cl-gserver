(in-package :sento.atomic)

;; ---------- reference -------------

(defstruct (atomic-reference
            (:constructor %make-atomic-reference (cell lock)))
  "Wrapper atomics package."
  cell
  lock)

(defun make-atomic-reference (&key (value nil))
  (%make-atomic-reference value (bt:make-lock)))

(defmethod print-object ((ref atomic-reference) stream)
  (print-unreadable-object (ref stream :type t :identity t)
    (format stream "~S" (atomic-get ref))))

(defmacro atomic-place (ref)
  `(slot-value ,ref 'cell))

(defmethod atomic-cas ((ref atomic-reference) old new)
  (declare (ignore old))
  (bt:with-lock-held ((atomic-reference-lock ref))
    (setf (atomic-place ref) new)))

(defmethod atomic-get ((ref atomic-reference))
  (atomic-place ref))

(defmethod atomic-swap ((ref atomic-reference) fn &rest args)
  (let ((old (atomic-get ref)))
    (atomic-cas ref old (apply fn old args))))

;; --------------- integer/long --------------

(defstruct (atomic-integer
            (:constructor %make-atomic-integer (cell lock)))
  "Wrapper atomics package."
  cell
  lock)

(defun make-atomic-integer (&key (value 0))
  (%make-atomic-reference value (bt:make-lock)))

(defmethod print-object ((int atomic-integer) stream)
  (print-unreadable-object (int stream :type t :identity t)
    (format stream "~S" (atomic-get int))))

(defmethod atomic-get ((int atomic-integer))
  (atomic-place int))

(defmethod atomic-cas ((int atomic-integer) old new)
  (declare (ignore old))
  (bt:with-lock-held ((atomic-reference-lock int))
    (setf (atomic-place int) new)))

(defmethod atomic-swap ((int atomic-integer) fn &rest args)
  (let ((old (atomic-get int)))
    (atomic-cas int old (apply fn old args))))
