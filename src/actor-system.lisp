
(in-package :cl-gserver.actor-system)

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
                    wt:make-wheel-timer))

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
   (eventstream :initform nil
                :reader evstream
                :documentation
                "The system event stream. See `ev:eventstream` for more info.")
   (timeout-timer :initform nil
                  :reader timeout-timer
                  :documentation
                  "A wheel-timer used for timeouts to make timeouts less resource expensive."))
  (:documentation
   "An `actor-system` is the opening facility. The first thing you do is to create an `actor-system` using the main constructor `make-actor-system`.
With the `actor-system` you can create actors via the `ac:actor-context` protocol function: `ac:actor-of`.

Or even simpler via `act:actor-of` which is a convenience macro:

```elisp
(act:actor-of (*system*)
                (lambda (self msg state)
                  ;; do stuff
                  (cons \"done\" state)))
```
"))

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
      (%register-timeout-timer system (%get-timeout-timer-config system-config)))
    (lf:linfo system)
    system))

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
                  &key receive
                    init
                    destroy
                    dispatcher
                    state
                    (type 'act:actor)
                    name
                    (context-key :user))
  "Private API to create system actors. Context-key is either `:internal` or `:user`
Users should use `actor-of`."
  (ac:actor-of (actor-context-for-key context-key system)
    :receive receive
    :init init
    :destroy destroy
    :dispatcher dispatcher
    :state state
    :type type
    :name name))

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
                     &key receive
                       (init nil) (destroy nil)
                       (dispatcher :shared) (state nil)
                       (type 'act:actor) (name nil))
  "See `ac:actor-of`"
  (%actor-of system
    :receive receive
    :init init
    :destroy destroy
    :dispatcher dispatcher
    :state state
    :type type
    :name name
    :context-key :user))

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
  (wt:shutdown-wheel-timer (timeout-timer self))
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
