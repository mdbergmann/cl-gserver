(defpackage :cl-gserver.actor-context
  (:use :cl)
  (:nicknames :ac)
  (:export #:actor-context
           #:actor-of
           #:find-actors
           #:all-actors
           #:stop
           #:shutdown
           #:system))
(in-package :cl-gserver.actor-context)

(defclass actor-context ()
  ((actors :initform (make-array 50 :adjustable t :fill-pointer 0)
           :reader actors
           :documentation
           "A list of actors.
This is internal API. Use `all-actors' or `find-actors' instead.")
   (system :initform nil
           :reader system
           :documentation
           "A reference to the `actor-system'."))
  (:documentation "`actor-context' deals with creating and maintaining actors.
The `actor-system' and the `actor' itself are composed of an `actor-context'."))

(defgeneric actor-of (actor-context create-fun &key dispatch-type)
  (:documentation "Creates and adds actors to the given context.
Specify the dispatcher type (`disp-type') as either:
`:shared' to have this actor use the shared message dispatcher of the context
`:pinned' to have this actor run it's own message box thread (faster, but more resource are bound.)"))

(defgeneric find-actors (actor-context find-fun)
  (:documentation "Returns actors where `find-fun' provides 'truth'."))

(defgeneric all-actors (actor-context)
  (:documentation "Retrieves all actors as a list"))

(defgeneric stop (actor-context actor)
  (:documentation "Stops the given actor on the context"))

(defgeneric shutdown (actor-context)
  (:documentation "Stops all actors in this context."))
