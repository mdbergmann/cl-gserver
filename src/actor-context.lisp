(defpackage :cl-gserver.actor-context
  (:use :cl)
  (:nicknames :ac)
  (:export #:actor-context
           #:make-actor-context
           #:actor-of
           #:get-actors
           #:add-actor))
(in-package :cl-gserver.actor-context)

(defclass actor-context ()
  ((actors :initform '()
           :reader get-actors
           :documentation "A list of actors."))
  (:documentation "Actor context deals with creating and adding actors in classes that inherit `actor-context'."))

(defun make-actor-context ()
  (make-instance 'actor-context))


(defgeneric actor-of (actor-context create-fun))

(defmethod actor-of ((self actor-context) create-fun)
  (let ((created (funcall create-fun)))
    (when created
      (add-actor self created))))


(defgeneric add-actor (actor-context actor))

(defmethod add-actor ((self actor-context) actor)
  (with-slots (actors) self
    (setf actors (list* actor actors))))
