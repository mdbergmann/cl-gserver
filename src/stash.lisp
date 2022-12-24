(defpackage :sento.stash
  (:use :cl)
  (:nicknames :stash)
  (:export #:stashing
           #:stash)
  )

(in-package :sento.stash)

(defclass stashing ()
  ((stashed-messages :initform '()
                     :reader stashed-messages))
  (:documentation ""))

(defgeneric stash (stashing msg)
  (:documentation ""))

(defmethod stash ((self stashing) msg)
  (with-slots (stashed-messages) self
    (setf stashed-messages (cons msg stashed-messages))))
