(defpackage :sento.stash
  (:use :cl)
  (:nicknames :stash)
  (:export #:stashing
           #:stash
           #:unstash-all)
  )

(in-package :sento.stash)

(defclass stashing ()
  ((stashed-messages :initform '()
                     :reader stashed-messages))
  (:documentation ""))

(defun stash (stashing msg)
  (with-slots (stashed-messages) stashing
    (setf stashed-messages (cons msg stashed-messages))))

(defun unstash-all (stashing)
  (assert (typep stashing 'act:actor) nil "Not an actor")
  (with-slots (stashed-messages) stashing
    (loop :for msg :in stashed-messages
          :do
             (act-cell::submit-message stashing msg nil nil nil))
    (setf stashed-messages '()))
  t)
