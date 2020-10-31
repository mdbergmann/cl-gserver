(defpackage :cl-gserver.actor-context
  (:use :cl)
  (:nicknames :ac)
  (:export #:make-actor-context))

(in-package :cl-gserver.actor-context)

(defun make-actor-context (actor-system)
  "Creates an `actor-context'. Requires a reference to `system'."
  (assert (not (null actor-system)) nil "Requires an actor-system!")
  (let ((context (make-instance 'actor-context)))
    (with-slots (system) context
      (setf system actor-system))
    context))

(defun get-shared-dispatcher (system)
  (getf (asys:dispatchers system) :shared))

(defun add-actor (context actor)
  (vector-push-extend actor (actors context))
  actor)

(defun message-box-for-dispatch-type (dispatch-type context)
  (case dispatch-type
    (:pinned (make-instance 'mesgb:message-box/bt))
    (otherwise (make-instance 'mesgb:message-box/dp
                              :dispatcher (get-shared-dispatcher (system context))
                              :max-queue-size 0))))

(defun make-actor (context create-fun dispatch-type)
  (let ((actor (funcall create-fun)))
    (when actor
      (setf (act-cell:msgbox actor) (message-box-for-dispatch-type dispatch-type context))
      (setf (act:context actor) (make-actor-context (system context))))
    actor))

(defmethod actor-of ((self actor-context) create-fun &key (dispatch-type :shared))
  (let ((created (make-actor self create-fun dispatch-type)))
    (when created
      (add-actor self created))))

(defmethod find-actors ((self actor-context) test-fun)
  (utils:filter test-fun (all-actors self)))

(defmethod all-actors ((self actor-context))
  (coerce (actors self) 'list))

(defmethod stop ((self actor-context) actor)
  (act-cell:stop actor))

(defmethod shutdown ((self actor-context))
  (dolist (actor (all-actors self))
    (act-cell:stop actor)))
