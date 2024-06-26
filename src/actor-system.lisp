
(in-package :sento.actor-system)

(eval-when (:compile-toplevel)
  (shadowing-import '(disp:make-dispatcher
                      disp:make-dispatcher-worker
                      ac:make-actor-context
                      ac:actor-of
                      ac:find-actors
                      ac:all-actors
                      ac:shutdown
                      ac:stop
                      ev:subscribe
                      ev:unsubscribe
                      ev:publish
                      wt:wheel-timer
                      wt:make-wheel-timer)))

(defclass actor-system ()
  ((dispatchers :initform '()
                :reader dispatchers
                :documentation
                "Internal API: contains a list of available message dispatchers.")
   (config :initform '()
           :accessor config
           :documentation
           "Internal API: the actor-system configuration.")
   (internal-actor-context :initform nil
                           :reader internal-actor-context
                           :documentation
                           "Internal API: an actor context reserved for agents/actors used by the system.")
   (user-actor-context :initform nil
                       :reader user-actor-context
                       :documentation
                       "Internal API: an actor context for agents/actors created by the user.")
   (timeout-timer :initform nil
                  :reader timeout-timer
                  :documentation
                  "Internal API: A wheel-timer used for timeouts to make timeouts less resource expensive.
This shouldn't be used freely. It is used internally by the system to support `act:ask` timeouts.")
   (eventstream :initform nil
                :reader evstream
                :documentation
                "The system event stream. See `ev:eventstream` for more info.")
   (scheduler :initform nil
              :reader scheduler
              :documentation
              "A general purpose scheduler that can be used by actors.
See `wt:wheel-timer` for more info.

The scheduler defaults to a resolution of 100 milliseconds and a maximum of 500 entries.

It is possible to disable the scheduler, i.e. to safe a thread resource, by setting the `:enabled` key to `:false` in the `:scheduler` section of the configuration."))
  (:documentation
   "An `actor-system` is the opening facility. The first thing you do is to create an `actor-system` using the main constructor `make-actor-system`.
With the `actor-system` you can create actors via the `ac:actor-context` protocol function: `ac:actor-of`."))

(defmethod print-object ((obj actor-system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (config internal-actor-context user-actor-context) obj
      (format stream "config: ~a, user actors: ~a, internal actors: ~a"
              config
              (length (ac:all-actors user-actor-context))
              (length (ac:all-actors internal-actor-context))))))

(defmethod initialize-instance :after ((self actor-system) &key)
  (with-slots (user-actor-context internal-actor-context) self
    (setf user-actor-context (ac:make-actor-context self "/user"))
    (setf internal-actor-context (ac:make-actor-context self "/internal"))))

(defun make-actor-system (&optional config)
  "Creates an `actor-system`.

Allows to provide an optional configuration. See `asys:*default-config*`.
If no config is provided the default config is used.
Is a config provided then it is merged with the default config.
Config options in the existing config override the default config.
See `config:config-from`."
  (let ((system-config (config:merge-config config *default-config*))
        (system (make-instance 'actor-system)))
    (with-slots (internal-actor-context) system
      (%register-config system system-config)
      (%register-dispatchers system (%get-dispatcher-config system-config) internal-actor-context)
      (%register-eventstream system (%get-eventstream-config system-config) internal-actor-context)
      (%register-timeout-timer system (%get-timeout-timer-config system-config))
      (%register-scheduler system (%get-scheduler-config system-config)))
    (log:info system)
    system))

(defun %get-scheduler-config (config)
  (config:retrieve-section config :scheduler))

(defun %get-timeout-timer-config (config)
  (config:retrieve-section config :timeout-timer))

(defun %get-dispatcher-config (config)
  (config:retrieve-section config :dispatchers))

(defun %get-eventstream-config (config)
  (config:retrieve-section config :eventstream))

(defun %register-config (system new-config)
  (with-slots (config) system
    (setf config new-config)))

(defun %register-eventstream (system ev-config actor-context)
  (with-slots (eventstream) system
    (setf eventstream (apply #'ev:make-eventstream
                             actor-context
                             ev-config))))

(defun %register-timeout-timer (system timer-config)
  (with-slots (timeout-timer) system
    (setf timeout-timer (apply #'wt:make-wheel-timer
                               timer-config))))

(defun %register-scheduler (system scheduler-config)
  (when (eq :true (getf scheduler-config :enabled))
    (with-slots (scheduler) system
      (setf scheduler (apply #'wt:make-wheel-timer
                             scheduler-config)))))

(defun %register-dispatchers (system dispatcher-config actor-context)
  "Creates a plist of dispatchers for the `:dispatchers` configuration section."
  (loop :for dispatcher-key :in (config:retrieve-keys dispatcher-config)
        :for dispatcher-section = (config:retrieve-section dispatcher-config dispatcher-key)
        :do (register-dispatcher system
                                 (apply #'disp:make-dispatcher
                                        actor-context
                                        dispatcher-key
                                        dispatcher-section))))

(defun actor-context-for-key (context-key system)
  (case context-key
    (:internal (internal-actor-context system))
    (otherwise (user-actor-context system))))

(defun %actor-of (system
                  &rest rest
                  &key receive
                       init
                       destroy
                       dispatcher
                       state
                       (type 'act:actor)
                       name
                       (context-key :user)
                       (queue-size nil)
                  &allow-other-keys)
  "Private API to create system actors. Context-key is either `:internal` or `:user`
Users should use `actor-of`."
  (alexandria:remove-from-plistf rest
                                 :context-key)
  (apply #'ac:actor-of
         (actor-context-for-key context-key system)
         :receive receive
         :init init
         :destroy destroy
         :dispatcher dispatcher
         :state state
         :type type
         :name name
         :queue-size queue-size
         rest))

(defun %find-actors (system path &key test key context-key)
  "Private API to find actors in both contexts the actor-system supports.
Users should use `ac:find-actors`."
  (let* ((root-path (format nil "/~a/" (string-downcase (symbol-name context-key))))
         (effective-path (if (str:starts-with-p "/" path)
                             (str:replace-first root-path "" path)
                             path)))
    (ac:find-actors
     (actor-context-for-key context-key system)
     effective-path
     :test test
     :key key)))

(defun %all-actors (system context-key)
  (ac:all-actors (actor-context-for-key context-key system)))

;; ----------------------------------------
;; Public Api
;; ----------------------------------------

(defun register-dispatcher (system dispatcher)
  "Registers a dispatcher to the actor-system.

- `system`: the actor-system
- `dispatcher`: the dispatcher instance."
  (with-slots (dispatchers) system
    (setf dispatchers (append (list (disp:identifier dispatcher) dispatcher) dispatchers))))

(defun register-new-dispatcher (system dispatcher-id &key workers strategy)
  "Makes and registers a new dispatcher.

- `system`: the actor-system
- `dispatcher-id`: the dispatcher identifier. Usually a global symbol like `:foo`
- `:workers`: key argument for the number of workers.
- `:strategy`: key argument for the dispatcher strategy (:random or :round-robin)"
  (register-dispatcher system
                       (disp:make-dispatcher (actor-context-for-key :internal system)
                                             dispatcher-id
                                             :workers workers
                                             :strategy strategy)))

;; ----------------------------------------
;; Public Api / actor-context protocol
;; ----------------------------------------

(defmethod actor-of ((system actor-system)
                     &rest rest
                     &key receive
                          (init nil)
                          (destroy nil)
                          (dispatcher :shared)
                          (state nil)
                          (type 'act:actor)
                          (name nil)
                     &allow-other-keys)
  "See `ac:actor-of`"
  (apply #'%actor-of
         system
         :context-key :user
         :init init
         :receive receive
         :destroy destroy
         :dispatcher dispatcher
         :state state
         :type type
         :name name
         rest))

(defmethod find-actors ((self actor-system) path &key (test #'string=) (key #'act-cell:name))
  "See `ac:find-actors`"
  (%find-actors self path :test test :key key :context-key :user))

(defmethod all-actors ((self actor-system))
  "See `ac:all-actors`"
  (%all-actors self :user))

(defmethod stop ((self actor-system) actor &key (wait nil))
  "See `ac:stop`"
  (act-cell:stop actor wait))

(defmethod shutdown ((self actor-system) &key (wait nil))
  "See `ac:shutdown`"
  (with-slots (timeout-timer scheduler) self
    (wt:shutdown-wheel-timer timeout-timer)
    (setf timeout-timer nil)
    (when scheduler
      (wt:shutdown-wheel-timer scheduler)
      (setf scheduler nil)))
  (ac:shutdown (user-actor-context self) :wait wait)
  (ac:shutdown (internal-actor-context self) :wait wait))

;; ----------------------------------------
;; Public Api / eventcontext protocol
;; ----------------------------------------

(defmethod subscribe ((system actor-system) (subscriber act:actor) &optional pattern)
  "Convenience. Allows to subscribe to `ev:eventstream` by just providing the `asys:actor-system`."
  (ev:subscribe (evstream system) subscriber pattern))

(defmethod unsubscribe ((system actor-system) (unsubscriber act:actor))
  "Convenience. Allows to unsubscribe to `ev:eventstream` by just providing the `asys:actor-system`."
  (ev:unsubscribe (evstream system) unsubscriber))

(defmethod publish ((system actor-system) message)
  "Convenience. Allows to publish to `ev:eventstream` by just providing the `asys:actor-system`."
  (ev:publish (evstream system) message))
