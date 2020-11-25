(defpackage :cl-gserver.router
  (:use :cl)
  (:nicknames :router)
  (:export #:router
           #:make-router)
  )

(in-package :cl-gserver.router)

(defun make-router ()
  "Default constructor of router."
  (make-instance 'router))

(defclass router () ()
  (:documentation
   "A router combines a number of actors and implements the actor-api protocol.
A router strategy defines how one of the actors is determined as the forwarding target of the message."))

