(defpackage :sento.stash
  (:use :cl)
  (:nicknames :stash)
  (:import-from #:act-cell
                #:*sender*
                #:submit-message)
  (:export #:stashing
           #:has-stashed-messages-p
           #:stash
           #:unstash-all)
  )

(in-package :sento.stash)

(defclass stashing ()
  ((stashed-messages :initform '()
                     :reader stashed-messages
                     :documentation "Stash is an unbounded list.
Stash items are a tuple (alist) of `msg' and `sender'."))
  (:documentation ""))

(defun has-stashed-messages-p (stashing)
  (with-slots (stashed-messages) stashing
    (not (null (car stashed-messages)))))

(defun stash (stashing msg)
  (with-slots (stashed-messages) stashing
    (setf stashed-messages
          (cons `(,msg . ,*sender*) stashed-messages)))
  t)

(defun unstash-all (stashing)
  (assert (typep stashing 'act:actor) nil "Not an actor")
  (with-slots (stashed-messages) stashing
    (loop :for amsg :in (reverse stashed-messages)
          :for msg = (car amsg)
          :for sender = (cdr amsg)
          :do
             ;; `submit-message' is internal API but can be used here
             ;; to implement this functionality
             (submit-message stashing msg nil sender nil))
    (setf stashed-messages '()))
  t)
