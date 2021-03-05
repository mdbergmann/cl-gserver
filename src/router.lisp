(defpackage :cl-gserver.router
  (:use :cl)
  (:nicknames :router)
  (:import-from #:act
                #:tell
                #:ask-s
                #:ask)
  (:export #:router
           #:make-router
           #:add-routee
           #:stop
           #:routees
           #:strategy-fun
           #:tell
           #:ask-s
           #:ask))

(in-package :cl-gserver.router)

(defun make-random-strategy ()
  "The default, built-in strategy: random."
  (lambda (len) (random len)))

(defun make-round-robin-strategy ()
  "Returns a let-over-lambda that implements a round-robin strategy."
  (let ((index 0))
    (lambda (len)
      (incf index)
      (if (= len index)
          (setf index 0))
      index)))

(defparameter *built-in-strategies*
  (list
   :random (make-random-strategy)
   :round-robin (make-round-robin-strategy)))

(defun get-strategy-fun (strategy)
  (cond
    ((eq :random strategy) (getf *built-in-strategies* strategy))
    ((eq :round-robin strategy) (getf *built-in-strategies* strategy))
    ((functionp strategy) strategy)
    (t (error "Unknown strategy!"))))

(defun make-router (&key (strategy :random) (routees nil))
  "Default constructor of router.
Built-in strategies: `:random`, `:round-robin`.
Specify your own strategy by providing a function that takes a `fixnum` as parameter 
and returns a `fixnum` that represents the index of the routee to choose.
Specify `routees` if you know them upfront."
  (let ((router (make-instance 'router
                               :strategy-fun (get-strategy-fun strategy))))
    (when routees
      (dolist (routee routees)
        (add-routee router routee)))
    router))

(defclass router ()
  ((routees :initform (make-array 2 :adjustable t :fill-pointer 0)
            :documentation "The routees.")
   (strategy-fun :initform (get-strategy-fun :random)
                 :initarg :strategy-fun
                 :reader strategy-fun
                 :documentation
                 "The router strategy function.
The `strategy` is a function with a `fixnum` as input and a `fixnum` as output.
The input represents the number of routees.
The output represents the index of the routee to choose by calling the function."))
  (:documentation
   "A router combines a pool of actors and implements the actor-api protocol.
So a `tell`, `ask-s` and `ask` is delegated to one of the routers routees.
While a router implements parts of the actor protocol it doesn't implement all.
I.e. a router cannot be `watch`ed.
A router `strategy` defines how one of the actors is determined as the forwarding target of the message."))

(defun add-routee (router routee)
  "Adds a routee/actor to the router."
  (vector-push-extend routee (slot-value router 'routees))
  routee)

(defun stop (router)
  "Stops all routees."
  (mapcar #'act-cell:stop (coerce (routees router) 'list)))

(defun get-strategy-index (router)
  (let* ((routees (slot-value router 'routees))
         (strategy-fun (strategy-fun router))
         (actor-index (funcall strategy-fun (length routees))))
    (log:debug "Using index from strategy: ~a" actor-index)
    actor-index))

(defun routees (router)
  "Returns the routees as list."
  (copy-list (coerce (slot-value router 'routees) 'list)))

(defmethod tell ((self router) message &optional sender)
  (tell
   (elt (slot-value self 'routees) (get-strategy-index self))
   message
   sender))

(defmethod ask-s ((self router) message &key time-out)
  (ask-s
   (elt (slot-value self 'routees) (get-strategy-index self))
   message
   :time-out time-out))

(defmethod ask ((self router) message &key time-out)
  (ask
   (elt (slot-value self 'routees) (get-strategy-index self))
   message
   :time-out time-out))
