(in-package :cl-gserver.actor-context)

(defclass actor-context ()
  ((id :initarg :id
       :initform nil
       :reader id
       :documentation
       "The id of this actor-context. Usually a string.")
   (actors :initform (atomic:make-atomic-reference :value '())
           :documentation
           "A list of actors.
This is internal API. Use `all-actors` or `find-actors` instead.")
   (system :initform nil
           :reader system
           :documentation
           "A reference to the `actor-system`."))
  (:documentation "`actor-context` deals with creating and maintaining actors.
The `actor-system` and the `actor` itself are composed of an `actor-context`."))

;; --------------------------------------
;; private functions
;; --------------------------------------

(defmethod actors ((self actor-context))
  (atomic:atomic-get (slot-value self 'actors)))

(defun %get-shared-dispatcher (system identifier)
  (getf (asys:dispatchers system) identifier))

(defun %add-actor (context actor)
  (let ((atomic-actors (slot-value context 'actors))
        (actors (actors context)))
    (if (atomic:atomic-cas atomic-actors actors (cons actor actors))
        actor
        (error "Unable to add actor!"))))

(defun %remove-actor (context actor)
  (let ((atomic-actors (slot-value context 'actors))
        (actors (actors context)))
    (atomic:atomic-cas atomic-actors
                       actors
                       (remove-if (lambda (a)
                                    (or (eq a actor)
                                        (string= (act-cell:name a)
                                                 (act-cell:name actor))))
                                  actors))))

(defun %message-box-for-dispatcher-id (context dispatcher-id queue-size)
  (case dispatcher-id
    (:pinned (make-instance 'mesgb:message-box/bt))
    (otherwise (let ((dispatcher (%get-shared-dispatcher (system context) dispatcher-id)))
                 (unless dispatcher
                   (error (format nil "No such dispatcher identifier '~a' exists!" dispatcher-id)))
                 (make-instance 'mesgb:message-box/dp
                                :dispatcher dispatcher
                                :max-queue-size queue-size)))))

(defun %verify-actor (context actor)
  "Checks certain things on the actor before it is attached to the context."
  (let* ((actor-name (act-cell:name actor))
         (exists-actor-p (find-actor-by-name context actor-name)))
    (when exists-actor-p
      (log:error "Actor with name '~a' already exists!" actor-name)
      (error (make-condition 'actor-name-exists :name actor-name)))))

(defun %create-actor (context create-fun dispatcher-id queue-size)
  (let ((actor (funcall create-fun)))
    (when actor
      (%verify-actor context actor)
      (act::initialize-with actor
       (%message-box-for-dispatcher-id context dispatcher-id queue-size)
       (make-actor-context (system context)
                           (utils:mkstr (id context) "/" (act-cell:name actor)))))
    actor))

;; --------------------------------------
;; public interface
;; --------------------------------------

(defun make-actor-context (actor-system &optional (id nil))
  "Creates an `actor-context`. Requires a reference to `actor-system`
`id` is an optional value that can identify the `actor-context`.
Creating an actor-context manually is usually not needed.
An `asys:actor-system` implements the `actor-context` protocol.
An `act:actor` contains an `actor-context`."
  (let ((context (make-instance 'actor-context :id id)))
    (with-slots (system) context
      (setf system actor-system))
    context))

(defmethod actor-of ((self actor-context) create-fun &key (dispatcher-id :shared) (queue-size 0))
  "See `ac:actor-of`"
  (let ((created (%create-actor self create-fun dispatcher-id queue-size)))
    (when created
      (act:watch created self)
      (%add-actor self created))))

(defmethod find-actors ((self actor-context) test-fun)
  "See `ac:find-actors`"
  (utils:filter test-fun (all-actors self)))

(defmethod find-actor-by-name ((self actor-context) name)
  "See `ac:find-actor-by-name`"
  (find-if (lambda (a)
             (let ((seq-name (act-cell:name a)))
               (or (eq name seq-name)
                   (string= name seq-name))))
           (actors self)))

(defmethod all-actors ((self actor-context))
  "See `ac:all-actors`"
  (actors self))

(defmethod stop ((self actor-context) actor)
  "See `ac:stop`"
  (act-cell:stop actor))

(defmethod shutdown ((self actor-context))
  "See `ac:shutdown`"
  (dolist (actor (all-actors self))
    (act-cell:stop actor)))

(defmethod notify ((self actor-context) actor notification)
  (case notification
    (:stopped (%remove-actor self actor))))
