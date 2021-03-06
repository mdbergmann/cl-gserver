
(in-package :cl-gserver.actor-system)

(shadowing-import '(disp:make-dispatcher
                    disp:make-dispatcher-worker
                    ac:make-actor-context
                    ac:actor-of
                    ac:find-actors
                    ac:find-actor-by-name
                    ac:all-actors
                    ac:shutdown
                    ac:stop))

(defclass actor-system ()
  ((dispatchers :initform '()
                :reader dispatchers
                :documentation
                "Internal API: contains a list of available message dispatchers.")
   (internal-actor-context :initform nil
                           :reader internal-actor-context
                           :documentation
                           "Internal API: an actor context reserved for agents/actors used by the system.")
   (user-actor-context :initform nil
                       :reader user-actor-context
                       :documentation
                       "Internal API: an actor context for agents/actors created by the user."))
  (:documentation
   "An `actor-system` is the opening facility. The first thing you do is to create an `actor-system` using the main constructor `make-actor-system`.
With the `actor-system` you can create actors via the `actor-context` protocol function: `actor-of`."))

(defmethod print-object ((obj actor-system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatchers internal-actor-context user-actor-context) obj
      (format stream "shared-workers: ~a, user actors: ~a, internal actors: ~a"
              (length (disp:workers (getf dispatchers :shared)))
              (length (ac:all-actors user-actor-context))
              (length (ac:all-actors internal-actor-context))))))

(defmethod initialize-instance :after ((self actor-system) &key)
  (with-slots (user-actor-context internal-actor-context) self
    (setf user-actor-context (ac:make-actor-context self "/user"))
    (setf internal-actor-context (ac:make-actor-context self "/internal"))))

(defun make-actor-system (&key (shared-dispatcher-workers 4))
  "Creates an `actor-system`.
Allows to configure the amount of workers for the `shared-dispatcher`."
  (let ((system (make-instance 'actor-system)))
    (with-slots (dispatchers) system
      (setf dispatchers (list :shared (disp:make-dispatcher
                                       :num-workers shared-dispatcher-workers))))
    (log:info system)
    system))

;; ----------------------------------------
;; Private Api
;; ----------------------------------------

(defun actor-context-for-key (context-key system)
  (case context-key
    (:internal (internal-actor-context system))
    (otherwise (user-actor-context system))))

(defun %actor-of (system create-fun dispatch-type &key (context-key :user) (queue-size 0))
  "Private API to create system actors. Context-key is either `:internal` or `:user`
Users should use `actor-of`."
  (ac:actor-of (actor-context-for-key context-key system)
    create-fun
    :dispatch-type dispatch-type
    :queue-size queue-size))

(defun %find-actors (system test-fun &key context-key)
  "Private API to find actors in both contexts the actor-system supports.
Users should use `find-actors`."
  (ac:find-actors (actor-context-for-key context-key system) test-fun))

(defun %find-actor-by-name (system name &key context-key)
  "Private API to find an actor by name in the specified context."
  (ac:find-actor-by-name (actor-context-for-key context-key system) name))

(defun %all-actors (system context-key)
  (ac:all-actors (actor-context-for-key context-key system)))

;; ----------------------------------------
;; Public Api / actor-context protocol
;; ----------------------------------------

(defmethod actor-of ((self actor-system) create-fun &key (dispatch-type :shared) (queue-size 0))
  "See `ac:actor-of`"
  (%actor-of self create-fun dispatch-type :context-key :user :queue-size queue-size))

(defmethod find-actors ((self actor-system) test-fun)
  "See `ac:find-actors`"
  (%find-actors self test-fun :context-key :user))

(defmethod find-actor-by-name ((self actor-system) name)
  "See `ac:find-actor-by-name`"
  (%find-actor-by-name self name :context-key :user))

(defmethod all-actors ((self actor-system))
  "See `ac:all-actors`"
  (%all-actors self :user))

(defmethod stop ((self actor-system) actor)
  "See `ac:stop`"
  (act-cell:stop actor))

(defmethod shutdown ((self actor-system))
  "See `ac:shutdown`"
  (disp:shutdown (getf (dispatchers self) :shared))
  (ac:shutdown (user-actor-context self))
  (ac:shutdown (internal-actor-context self)))
