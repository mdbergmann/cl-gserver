(defpackage :cl-gserver.actor-context
  (:use :cl)
  (:nicknames :ac)
  (:export #:actor-context
           #:make-actor-context
           #:actor-of
           #:add-actor
           #:find-actors
           #:all-actors))
(in-package :cl-gserver.actor-context)

(defclass actor-context ()
  ((actors :initform (make-array 50 :adjustable t :fill-pointer 0)
           :reader actors
           :documentation "A list of actors."))
  (:documentation "Actor context deals with creating and adding actors in classes that inherit `actor-context'."))

(defun make-actor-context ()
  (make-instance 'actor-context))

(defgeneric actor-of (actor-context create-fun &key disp-type)
  (:documentation "Creates and adds actors to the given context.
Specify the dispatcher type (`disp-type') as either:
`:shared' to have this actor use the shared messagfe dispatcher of the system
`:pinned' to have this actor run it's own message box thread (faster, but more resource are bound.)"))

(defgeneric add-actor (actor-context actor)
  (:documentation "Adds the actor to the context and returns it."))

(defgeneric find-actors (actor-context find-fun)
  (:documentation "Returns actors where `find-fun' provides 'truth'."))

(defgeneric all-actors (actor-context)
  (:documentation "Retrieves all actors as a list"))

;; --------------------------------
;; Actor context impl
;; --------------------------------

(defmethod add-actor ((self actor-context) actor)
  (vector-push-extend actor (actors self))
  actor)

(defmethod find-actors ((self actor-context) find-fun)
  (mapcan (lambda (x) (if (funcall find-fun x) (list x))) (all-actors self)))

(defmethod all-actors ((self actor-context))
  (coerce (actors self) 'list))
