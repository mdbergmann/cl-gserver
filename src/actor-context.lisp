
(in-package :cl-gserver.actor-context)

(defun make-actor-context (actor-system &optional (id nil))
  "Creates an `actor-context`. Requires a reference to `actor-system`
`id` is an optional value that can identify the `actor-context`."
  (assert (not (null actor-system)) nil "Requires an actor-system!")
  (let ((context (make-instance 'actor-context :id id)))
    (with-slots (system) context
      (setf system actor-system))
    context))

(defun get-shared-dispatcher (system)
  (getf (asys:dispatchers system) :shared))

(defun add-actor (context actor)
  (with-slots (actors) context
    (setf actors
          (hamt:dict-insert actors (act-cell:name actor) actor)))
  actor)

(defun remove-actor (context actor)
  (with-slots (actors) context
    (setf actors
          (hamt:dict-remove actors (act-cell:name actor)))))

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
      (let ((context-id (utils:mkstr (id context) "/" (act-cell:name actor))))
        (setf (act:context actor) (make-actor-context (system context) context-id))))
    actor))

(defmethod actor-of ((self actor-context) create-fun &key (dispatch-type :shared) (queue-size 0))
  (let ((created (create-actor self create-fun dispatch-type queue-size)))
    (when created
      (act:watch created self)
      (add-actor self created))))

(defmethod find-actors ((self actor-context) test-fun)
  (utils:filter test-fun (all-actors self)))

(defmethod find-actor-by-name ((self actor-context) name)
  (hamt:dict-lookup (actors self) name))

(defmethod all-actors ((self actor-context))
  (hamt:dict-reduce (lambda (acc key val)
                      (declare (ignore key))
                      (cons val acc))
                    (actors self)
                    '()))

(defmethod stop ((self actor-context) actor)
  (act-cell:stop actor))

(defmethod notify ((self actor-context) actor notification)
  (case notification
    (:stopped (remove-actor self actor))))

(defmethod shutdown ((self actor-context))
  (dolist (actor (all-actors self))
    (act-cell:stop actor)))

