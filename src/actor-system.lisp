(defpackage :cl-gserver.actor-system
  (:use :cl :cl-gserver.actor :cl-gserver.dispatcher
        :cl-gserver.actor-system-api :cl-gserver.actor-context)
  (:nicknames :system)
  (:import-from #:dispatcher
                #:shared-dispatcher
                #:make-dispatcher
                #:make-dispatcher-worker)
  (:export #:make-actor-system
           #:actor-system
           #:message-dispatcher))

(in-package :cl-gserver.actor-system)

(defclass actor-system (actor-context)
  ((dispatcher :initform nil
               :reader message-dispatcher
               :documentation "The message dispatcher."))
  (:documentation
   "A system is a container for `actors' or subclasses.
Create the `actor-system' using the constructor function `make-actor-system'.
It allows to create actors using the method `actor-of'."))

(defmethod print-object ((obj actor-system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatcher) obj
      (format stream "dispatcher: ~a" dispatcher))))

(defun make-actor-system (&key (shared-dispatcher-workers 4))
  "Creates a system.
Allows to configure the amount of workers for the `shared-dispatcher'."
  (let ((system (make-instance 'actor-system)))
    (with-slots (dispatcher) system
      (setf dispatcher (make-dispatcher
                        'shared-dispatcher
                        :num-workers shared-dispatcher-workers)))
    system))


(defmethod shutdown ((self actor-system))
  (dispatcher-api:shutdown (message-dispatcher self)))

;; -------------------------------------
;; actor-context impl
;; -------------------------------------

(defmethod actor-of ((self actor-system) create-fun &key (disp-type :shared))
  (let ((actor (funcall create-fun)))
    (assert (typep actor 'actor))
    (setf (act-cell:system actor) self)
    (setf (act-cell:msgbox actor) (message-box-for-disp-type disp-type self))
    (add-actor self actor)
    actor))

(defun message-box-for-disp-type (disp-type system)
  (case disp-type
    (:pinned (make-instance 'mesgb:message-box-bt))
    (otherwise (make-instance 'mesgb:message-box-dp
                              :dispatcher (message-dispatcher system)
                              :max-queue-size 0))))
