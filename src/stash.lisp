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
  (:documentation "`stashing` is a mixin class to `act:actor`.
It can 'stash' away arriving messages which should not be handled now, but later, after the actor is 'able' to handle them.
See function descriptions below.

The main use-case is for `act:tell` and `act:ask`. `act:ask-s` will not work.
timeouts are ignored because it is not clear how long stashed messages will reside in stash.
However the `sender`, if given (on `act:tell`), is preserved."))

(defun has-stashed-messages-p (stashing)
  "Are there any stashed messages?"
  (with-slots (stashed-messages) stashing
    (not (null (car stashed-messages)))))

(defun stash (stashing msg)
  "Stash `msg` for later unstash.
On stashing a message the actor should respond with: `(cons :no-reply state)`
to avoid returning a response to sender (if given)."
  (with-slots (stashed-messages) stashing
    (setf stashed-messages
          (cons `(,msg . ,*sender*) stashed-messages)))
  t)

(defun unstash-all (stashing)
  "Unstash all messages.
Messages are re-submitted to the actor in the order they were stashed.
Resubmitting means they are added to the end of the queue like any ordinary message would."
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
