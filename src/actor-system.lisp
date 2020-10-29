(defpackage :cl-gserver.actor-system
  (:use :cl)
  (:nicknames :asys)
  (:import-from #:disp
                #:shared-dispatcher
                #:make-dispatcher
                #:make-dispatcher-worker)
  (:import-from #:ac
                #:make-actor-context
                #:actor-of
                #:find-actors
                #:shutdown)
  (:export #:make-actor-system
           #:actor-system
           #:dispatchers))

(in-package :cl-gserver.actor-system)

(defclass actor-system ()
  ((dispatchers :initform nil
                :reader dispatchers
                :documentation "The message dispatcher.")
   (system-actor-context :initform nil
                         :reader system-actor-context
                         :documentation "An actor context reserved for agents/actors used by the system.")
   (user-actor-context :initform nil
                       :reader user-actor-context
                       :documentation "An actor context for agents/actors created by the user."))
  (:documentation
   "A system is a container for `actors' or subclasses.
Create the `actor-system' using the constructor function `make-actor-system'.
It allows to create actors using the method `actor-of'."))

(defmethod print-object ((obj actor-system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatchers) obj
      (format stream "dispatchers: ~a" dispatchers))))

(defmethod initialize-instance :after ((self actor-system) &key)
  (with-slots (user-actor-context system-actor-context) self
    (setf user-actor-context (make-actor-context self))
    (setf system-actor-context (make-actor-context self))))

(defun make-actor-system (&key (shared-dispatcher-workers 4))
  "Creates a system.
Allows to configure the amount of workers for the `shared-dispatcher'."
  (let ((system (make-instance 'actor-system)))
    (with-slots (dispatchers) system
      (setf dispatchers (list :shared (make-dispatcher
                                       'shared-dispatcher
                                       :num-workers shared-dispatcher-workers))))
    system))

;; ----------------------------------------
;; Private Api
;; ----------------------------------------

(defun actor-context-for-key (context-key system)
  (case context-key
    (:system (system-actor-context system))
    (otherwise (user-actor-context system))))

(defun %actor-of (system create-fun dispatch-type &key (context-key :user))
  "Private API to create system actors. Context-key is either `:system' or `:user'
Users should use `actor-of'."
  (ac:actor-of
   (actor-context-for-key context-key system)
   create-fun
   :dispatch-type dispatch-type))

(defun %find-actors (system find-fun &key context-key)
  "Private API to find actors in both contexts the actor-system supports.
Users should use `find-actors'."
  (ac:find-actors (actor-context-for-key context-key system) find-fun))

;; ----------------------------------------
;; Public Api
;; ----------------------------------------

(defmethod actor-of ((self actor-system) create-fun &key (dispatch-type :shared))
  (%actor-of self create-fun dispatch-type :context-key :user))

(defmethod find-actors ((self actor-system) find-fun)
  (%find-actors self find-fun :context-key :user))

(defmethod shutdown ((self actor-system))
  (disp:shutdown (getf (dispatchers self) :shared))
  (ac:shutdown (user-actor-context self))
  (ac:shutdown (system-actor-context self)))
