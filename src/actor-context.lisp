(in-package :sento.actor-context)

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

(defmethod actors ((context actor-context))
  (atomic:atomic-get (slot-value context 'actors)))

(defun %get-shared-dispatcher (system identifier)
  (getf (asys:dispatchers system) identifier))

(defun %add-actor (context actor)
  (let ((atomic-actors (slot-value context 'actors)))
    (atomic:atomic-swap atomic-actors (lambda (old-actors)
                                        (cons actor old-actors)))
    actor))

(defun %remove-actor (context actor)
  (let ((atomic-actors (slot-value context 'actors)))
    (atomic:atomic-swap atomic-actors
                        (lambda (old-actors)
                          (remove-if (lambda (a)
                                       (or (eq a actor)
                                           (string= (act-cell:name a)
                                                    (act-cell:name actor))))
                                     old-actors)))))

(defun %message-box-for-dispatcher-id (context dispatcher-id queue-size)
  (case dispatcher-id
    (:pinned (make-instance 'mesgb:message-box/bt))
    (otherwise (let ((dispatcher (%get-shared-dispatcher (system context) dispatcher-id)))
                 (unless dispatcher
                   (error (format nil "No such dispatcher identifier '~a' exists!" dispatcher-id)))
                 (make-instance 'mesgb:message-box/dp
                                :dispatcher dispatcher
                                :max-queue-size queue-size)))))

(defun %find-actor-by-name (context name)
  (find-if (lambda (a)
             (let ((seq-name (act-cell:name a)))
               (or (eq name seq-name)
                   (string= name seq-name))))
           (actors context)))

(defun %find-actors (context path &key test key)
  (let ((actors-to-search (all-actors context)))
    (miscutils:filter (lambda (x)
                    (funcall test path (funcall key x)))
                  actors-to-search)))

(defun %verify-actor (context actor)
  "Checks certain things on the actor before it is attached to the context."
  (let* ((actor-name (act-cell:name actor))
         (exists-actor-p (%find-actor-by-name context actor-name)))
    (when exists-actor-p
      (log:error "Actor with name '~a' already exists!" actor-name)
      (error (make-condition 'actor-name-exists :name actor-name)))))

(defun %create-actor (context create-fun dispatcher-id queue-size)
  (let ((actor (funcall create-fun)))
    (when actor
      (%verify-actor context actor)
      (act::finalize-initialization actor
       (%message-box-for-dispatcher-id context dispatcher-id queue-size)
       (make-actor-context (system context)
                           (miscutils:mkstr (id context) "/" (act-cell:name actor)))))
    actor))

(defun %actor-of (context create-fun &key (dispatcher :shared) (queue-size 0))
  "See `ac:actor-of`"
  (let ((created (%create-actor context create-fun dispatcher queue-size)))
    (when created
      (act:watch created context)
      (%add-actor context created))))

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

(defmethod actor-of ((context actor-context)
                     &rest rest
                     &key
                     receive
                     (init nil)
                     (destroy nil)
                     (dispatcher :shared)
                     (state nil)
                     (type 'act:actor)
                     (name nil)
                     (queue-size nil)
                     &allow-other-keys)
  "See `ac:actor-of`."
  (check-type receive function "a function!")
  (alexandria:remove-from-plistf rest
                                 :queue-size
                                 :dispatcher
                                 :init
                                 :destroy
                                 :state
                                 :type
                                 :name)
  (%actor-of context
             (lambda () (apply #'act:make-actor receive
                               :init init
                               :destroy destroy
                               :state state
                               :type type
                               :name name
                               rest))
             :dispatcher dispatcher
             :queue-size queue-size))

;; test 2-arity function with 'path' and 'act-cell-name' (default)
(defmethod find-actors ((context actor-context) path &key (test #'string=) (key #'act-cell:name))
  "See `ac:find-actors`"
  (if (str:starts-with-p "/" path)
      ;; root path, delegate to system
      (find-actors (system context) path :test test :key key)
      (let ((path-comps (str:split "/" path))
            (context context))
        (loop :for path-comp :in (butlast path-comps)
              :for actor = (find path-comp (all-actors context)
                                 :test #'string=
                                 :key #'act-cell:name)
              :do (if actor
                      (setf context (act:context actor))
                      (error (format nil "Cannot find path component ~a" path-comp))))
        (%find-actors context (car (last path-comps)) :test test :key key))))

(defmethod all-actors ((context actor-context))
  "See `ac:all-actors`"
  (actors context))

(defmethod stop ((context actor-context) actor &key (wait nil))
  "See `ac:stop`"
  (act-cell:stop actor wait))

(defmethod shutdown ((context actor-context) &key (wait nil))
  "See `ac:shutdown`"
  (dolist (actor (all-actors context))
    (act-cell:stop actor wait)))

(defmethod notify ((context actor-context) actor notification)
  (case notification
    (:stopped
     (progn
       (%remove-actor context actor)
       (log:debug "Actor removed: ~a" (act-cell:name actor))))))
