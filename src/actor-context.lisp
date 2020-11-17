
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

(defun remove-actor (context actor)
  (with-slots (actors) context
    (setf actors
          (delete-if (lambda (a) (eq a actor)) actors))))

(defun message-box-for-dispatch-type (context dispatch-type queue-size)
  (case dispatch-type
    (:pinned (make-instance 'mesgb:message-box/bt))
    (otherwise (make-instance 'mesgb:message-box/dp
                              :dispatcher (get-shared-dispatcher (system context))
                              :max-queue-size queue-size))))

(defun verify-actor (context actor)
  "Checks certain things on the actor before it is attached to the context."
  (let* ((actor-name (act-cell:name actor))
         (exists-actor-p (not (null (car
                                     (find-actors context
                                                  (lambda (a)
                                                    (string= (act-cell:name a) actor-name))))))))
    (when exists-actor-p
      (log:error "Actor with name '~a' already exists!" actor-name)
      (error (make-condition 'actor-name-exists :name actor-name)))))

(defun create-actor (context create-fun dispatch-type queue-size)
  (let ((actor (funcall create-fun)))
    (when actor
      (verify-actor context actor)
      (setf (act-cell:msgbox actor) (message-box-for-dispatch-type context dispatch-type queue-size))
      (setf (act:context actor) (make-actor-context (system context))))
    actor))

(defmethod actor-of ((self actor-context) create-fun &key (dispatch-type :shared) (queue-size 0))
  (let ((created (create-actor self create-fun dispatch-type queue-size)))
    (when created
      (act:watch created self)
      (add-actor self created))))

(defmethod find-actors ((self actor-context) test-fun)
  (utils:filter test-fun (all-actors self)))

(defmethod all-actors ((self actor-context))
  (coerce (actors self) 'list))

(defmethod stop ((self actor-context) actor)
  (act-cell:stop actor))

(defmethod notify ((self actor-context) actor notification)
  (case notification
    (:stopped (remove-actor self actor))))

(defmethod shutdown ((self actor-context))
  (dolist (actor (all-actors self))
    (act-cell:stop actor)))
