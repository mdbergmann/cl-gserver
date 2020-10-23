(defpackage :cl-gserver.actor-system
  (:use :cl :cl-gserver.actor :cl-gserver.dispatcher
        :cl-gserver.actor-system-api :cl-gserver.actor-context)
  (:nicknames :system)
  (:import-from #:dispatcher
                #:shared-dispatcher
                #:make-dispatcher
                #:make-dispatcher-worker)
  (:export #:make-actor-system
           #:actor-system))

(in-package :cl-gserver.actor-system)

(defclass actor-system (actor-context)
  ((dispatcher :initarg :dispatcher
               :initform nil
               :reader message-dispatcher
               :documentation "The message dispatcher."))
  (:documentation
   "A system is a container for `actors' or subclasses."))

(defmethod print-object ((obj actor-system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatcher) obj
      (format stream "dispatcher: ~a"
              dispatcher))))

(defun make-actor-system (&key
                      (dispatcher-type 'shared-dispatcher)
                      (dispatcher-workers 4))
  "Creates a system with 4 workers by default."
  (make-instance 'actor-system
                 :dispatcher (make-dispatcher
                              dispatcher-type
                              :num-workers dispatcher-workers)))

(defmethod shutdown ((self actor-system))
  (dispatcher-api:shutdown (message-dispatcher self)))

;; -------------------------------------
;; actor-context impl
;; -------------------------------------

(defmethod actor-of ((self actor-system) create-fun)
  (let ((actor (funcall create-fun)))
    (assert (subtypep (type-of actor) 'actor))
    (setf (act-cell:system (act:cell actor)) self)
    (add-actor self actor)
    actor))
