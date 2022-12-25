(defpackage :sento.stash
  (:use :cl)
  (:nicknames :stash)
  (:export #:stashing
           #:has-stashed-messages
           #:stash
           #:unstash-all)
  )

(in-package :sento.stash)

(defclass stashing ()
  ((stashed-messages :initform '()
                     :reader stashed-messages))
  (:documentation ""))

(defun has-stashed-messages (stashing)
  (with-slots (stashed-messages) stashing
    (not (null (car stashed-messages)))))

(defun stash (stashing msg)
  (with-slots (stashed-messages) stashing
    (setf stashed-messages
          (cons `(,msg . ,act-cell:*sender*) stashed-messages)))
  t)

(defun unstash-all (stashing)
  (assert (typep stashing 'act:actor) nil "Not an actor")
  (with-slots (stashed-messages) stashing
    (loop :for amsg :in stashed-messages
          :for msg = (car amsg)
          :for sender = (cdr amsg)
          :do
             ;; `submit-message' is internal API but can be used here
             ;; to implement this functionality
             (act-cell::submit-message stashing msg nil sender nil))
    (setf stashed-messages '()))
  t)
