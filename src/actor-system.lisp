(defpackage :cl-gserver.actor-system
  (:use :cl :cl-gserver.actor :cl-gserver.dispatcher :cl-gserver.actor-context
        :cl-gserver.actor-system-api)
  (:nicknames :system)
  (:import-from #:dispatcher
                #:shared-dispatcher
                #:make-dispatcher
                #:make-dispatcher-worker)
  (:export #:make-actor-system
           #:actor-system
           #:message-dispatcher))

(in-package :cl-gserver.actor-system)

(defclass actor-system ()
  ((dispatcher :initform nil
               :reader message-dispatcher
               :documentation "The message dispatcher.")
   (system-actor-context :initform (make-actor-context)
                         :reader system-actor-context
                         :documentation "An actor context reserved for agents/actors used by the system.")
   (user-actor-context :initform (make-actor-context)
                       :reader user-actor-context
                       :documentation "An actor context for agents/actors created by the user."))
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

;; ----------------------------------------
;; Private Api
;; ----------------------------------------

(defun message-box-for-disp-type (disp-type system)
  (case disp-type
    (:pinned (make-instance 'mesgb:message-box-bt))
    (otherwise (make-instance 'mesgb:message-box-dp
                              :dispatcher (message-dispatcher system)
                              :max-queue-size 0))))

(defun actor-context-for-key (context system)
  (case context
    (:system (system-actor-context system))
    (otherwise (user-actor-context system))))

(defun make-new-actor (system create-fun disp-type)
  (let ((actor (funcall create-fun)))
    (assert (typep actor 'actor))
    (setf (act-cell:system actor) system)
    (setf (act-cell:msgbox actor) (message-box-for-disp-type disp-type system))
    actor))

(defun %actor-of (system create-fun disp-type &key (context :user))
  "Private API to create system actors. Context-key is either `:system' or `:user'
Users should use `actor-of'."
  (let ((actor (make-new-actor system create-fun disp-type)))
    (add-actor (actor-context-for-key context system) actor)))

(defun %find-actors (system find-fun &key context)
  "Private API to find actors in both contexts the actor-system supports.
Users should use `find-actors'."
  (ac:find-actors (actor-context-for-key context system) find-fun))

;; ----------------------------------------
;; Public Api
;; ----------------------------------------

(defmethod actor-of ((self actor-system) create-fun &key (disp-type :shared))
  (%actor-of self create-fun disp-type :context :user))

(defmethod find-actors ((self actor-system) find-fun)
  (%find-actors self find-fun :context :user))

(defmethod shutdown ((self actor-system))
  (dispatcher-api:shutdown (message-dispatcher self))
  (dolist (actor (ac:all-actors (user-actor-context self)))
    (act-cell:stop actor))
  (dolist (actor (ac:all-actors (system-actor-context self)))
    (act-cell:stop actor)))
