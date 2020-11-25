(defpackage :cl-gserver.router
  (:use :cl)
  (:nicknames :router)
  (:export #:router
           #:make-router
           #:add-routee
           #:stop
           #:routees)
  )

(in-package :cl-gserver.router)

(defun make-router ()
  "Default constructor of router."
  (make-instance 'router))

(defclass router ()
  ((routees :initform (make-array 2 :adjustable t :fill-pointer 0)
            :reader routees
            :documentation "The routees."))
  (:documentation
   "A router combines a number of actors and implements the actor-api protocol.
So a `tell', `ask' and `async-ask' is delegated to one of the routerss routees.
A router strategy defines how one of the actors is determined as the forwarding target of the message."))

(defun add-routee (router routee)
  "Adds a routee/actor to the router."
  (vector-push-extend routee (routees router))
  routee)

(defun stop (router)
  "Stops all routees."
  (mapcar #'act-cell:stop (coerce (routees router) 'list)))
