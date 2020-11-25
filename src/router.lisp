(defpackage :cl-gserver.router
  (:use :cl)
  (:nicknames :router)
  (:export #:router
           #:make-router
           #:add-routee
           #:stop
           #:routees
           #:strategy)
  )

(in-package :cl-gserver.router)

(defun random-strategy (len)
  (random len))

(defparameter *built-in-strategies*
  (list :random #'random-strategy))

(defun get-strategy (strategy)
  (cond
    ((eq :random strategy) (getf *built-in-strategies* strategy))
    ((functionp strategy) strategy)
    (t (error "Unknown strategy!"))))

(defun make-router (&optional (strategy :random))
  "Default constructor of router.
Built-in strategies: `:random'.
Specify your own strategy by providing a function that takes a `fixnum' as parameter and returns a `fixnum' that represents the index of the routee to choose."
  (make-instance 'router
                 :strategy (get-strategy strategy)))

(defclass router ()
  ((routees :initform (make-array 2 :adjustable t :fill-pointer 0)
            :reader routees
            :documentation "The routees.")
   (strategy :initform (error "Strategy required!")
             :initarg :strategy
             :reader strategy
             :documentation
             "The router strategy.
The `strategy' is a function with a `fixnum' as input and a `fixnum' as output.
The inout represents the number of routees.
The output represents the index of the routee to choose."))
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
