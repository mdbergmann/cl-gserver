
(in-package :cl-gserver.actor)

(shadowing-import '(act-cell:actor-cell
                    act-cell:pre-start
                    act-cell:after-stop
                    act-cell:handle-call
                    act-cell:handle-cast
                    act-cell:stop
                    future:make-future
                    ev:subscribe
                    ev:unsubscribe
                    ev:publish
                    ac:find-actors
                    ac:find-actor-by-name
                    ac:all-actors))

(defclass actor (actor-cell)
  ((receive :initarg :receive
            :initform (error "'receive' must be specified!")
            :reader receive
            :documentation
            "`receive` is a function that has to take 3 parameters:
- `self`: the actor instance
- `msg`: the received message
- `state`: the current state of the actor
The `sender` of the message, if available, is accessible with `*sender*` from within
the receive function or a behavior.")
   (behavior :initform nil
             :documentation
             "Behavior function applied via `act:become` and reverted via `act:unbecome`
`act:behavior` function takes the same parameters as `act:receive`.")
   (init-fun :initarg :init
             :initform nil
             :documentation "Init hook.
Function is called when actor was initialized.
`act:context` is all setup at that time.
Parameter of the lambda is the actor itself.")
   (destroy-fun :initarg :destroy
                :initform nil
                :documentation "Destroy hook.
Function is called when actor was stopped.
Parameter of the lambda is the actor itself.")
   (context :initform nil
            :accessor context
            :documentation "The `ac:actor-context`")
   (watchers :initform '()
             :reader watchers
             :documentation "List of watchers of this actor."))
  (:documentation
   "This is the `actor` class.

The `actor` does its message handling using the `receive` function.

The `receive` function has to return a `cons` constructed of a message to be sent back to caller (`car`), if applicable, and the new state of the actor (as `cdr`).
I.e.: `(cons <my-response> <my-new-state>)`

There is asynchronous `tell` (no response), a synchronous `ask-s` and asynchronous `ask` which all can be used to send messages to the actor. The 'ask' variants provide a response from the actor where 'tell' is only fire-and-forget.

If the 'send' operation was `ask-s` or `ask` then the `car` part of the `cons` result will be sent back to the caller.
In case of a `tell` operation there will be no response and the `car` of the `cons` is ignored, if there is no sender (see `sender` argument to `tell`). If there is a sender defined (which must be an actor), then the `car` of the `cons` result is sent (using `tell`) to the sender.  
It is possible to specify `:no-reply` as `car` of `cons` in this case (`tell` with sender), which has the effect that the result is _not_ sent to the sender even if one exists. This is for the case that the user wants to handle the state and the notifications to a sender himself. It is useful when the message handling code for a particular message (in `receive`) should be executed in a special thread-pool, because long running operations within `receive` will block the message handling of the actor.

The `:no-reply` result works for `ask` and `tell`, because also `ask` is based on `tell`.
`ask-s` is really only useful if a synchronous result is required and should be avoided otherwise.

To stop an actors message processing in order to cleanup resouces you should `tell` (or `ask-s`) the `:stop` message. It will respond with `:stopped` (in case of `ask(-s)`)."))

;; --------------------------------------
;; Convenience macro for creating actors
;; --------------------------------------

(defmacro actor-of ((context
                     &optional (name nil))
                    &body body
                    &key receive (init nil) (dispatcher :shared) (state nil) (type ''actor))
  "Simple interface for creating an actor.

This is the preferred way to create an actor that runs within an `ac:actor-context`.

**!!! Attention:** this macro wraps the `act:make-actor` and `ac:actor-of` functionality to something more simple to use. 
Using this macro there is no need to use both `ac:actor-of` and `act:make-actor`.

`context` is either an `asys:actor-system`, an `ac:actor-context`, or an `act:actor` (any type of actor).
The new actor is created in the given context.

- `name` is optional. Specify when a static name is needed.
- `:receive` is required and must be a lambda with arguments 1. the actor, 2. the message, 3. the state
  Usually expressed as `(lambda (self msg state))`.
- `:init`: is an optional initialization function with one argument: the actor instance (self).
This represents a 'start' hook that is called after the actor was fully initialized.
- `:state` key can be used to initialize with a state.
- `:dispatcher` key can be used to define the message dispatcher manually.
  Options are `:shared` (default) and `:pinned`.
- `:type` can specify a custom actor class. See `act:make-actor` for more info."
  (declare (ignore body))
  (let ((unwrapped-context (gensym)))
    `(let* ((,unwrapped-context (etypecase ,context
                                  (asys:actor-system ,context)
                                  (ac:actor-context ,context)
                                  (act:actor (act:context ,context)))))
       (ac:actor-of ,unwrapped-context
         (lambda () (act:make-actor ,receive
                               :state ,state
                               :name ,name
                               :type ,type
                               :init ,init))
         :dispatcher-id ,dispatcher))))

(defmethod make-actor (receive &key name state (type 'actor) (init nil) (destroy nil))
  (make-instance type
                 :name name
                 :state state
                 :receive receive
                 :init init
                 :destroy destroy))

(defun initialize-with (actor message-box actor-context)
  "Private API: finalize initialization of the actor with a `mesgb:message-box` and an `ac:actor-context`."
  (setf (act-cell:msgbox actor) message-box)
  (setf (act:context actor) actor-context)
  (with-slots (init-fun) actor
    (when init-fun
      (funcall init-fun actor))))

(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (call-next-method obj string-stream)
      (format stream "path: ~a, cell: ~a"
              (path obj)
              (get-output-stream-string string-stream)))))

;; -------------------------------
;; actor-cell impls
;; -------------------------------

(defmethod handle-call ((self actor) message state)
  (with-slots (receive behavior) self
    (let ((effective-behavior (if behavior behavior receive)))
      (funcall effective-behavior self message state))))

(defmethod handle-cast ((self actor) message state)
  (with-slots (receive behavior) self
    (let ((effective-behavior (if behavior behavior receive)))
      (funcall effective-behavior self message state))))

(defmethod pre-start ((self actor) state)
  (declare (ignore state))
  (call-next-method))

(defmethod after-stop ((self actor))
  (call-next-method)
  (with-slots (destroy-fun) self
    (when destroy-fun
      (funcall destroy-fun self))))

;; -------- children handling ----------

(defun stop-children (actor)
  (let ((context (context actor)))
    (when context
      (dolist (child (ac:all-actors context))
        (stop child)))))

(defun notify-watchers-about-stop (actor)
  (dolist (watcher (watchers actor))
    (typecase watcher
      (act:actor (tell watcher (cons :stopped actor)))
      (ac:actor-context (ac:notify watcher actor :stopped)))))

;; -------------------------------
;; actor protocol impl
;; -------------------------------

(defmethod tell ((self actor) message &optional sender)
  (act-cell:cast self message sender))

(defmethod ask-s ((self actor) message &key (time-out nil))
  (act-cell:call self message :time-out time-out))

(defmethod become ((self actor) new-behavior)
  (with-slots (behavior) self
    (setf behavior new-behavior)))

(defmethod unbecome ((self actor))
  (with-slots (behavior) self
    (setf behavior nil)))

(defmethod path ((self actor))
  (when (context self)
    (ac:id (context self))))

(defmethod watch ((self actor) watcher)
  (with-slots (watchers) self
    (setf watchers (cons watcher watchers))))

(defmethod unwatch ((self actor) watcher)
  (with-slots (watchers) self
    (setf watchers (utils:filter (lambda (w) (not (eq watcher w))) watchers))))

(defmethod stop ((self actor))
  "If this actor has an `actor-context`, also stop all children.
In any case stop the actor-cell."
  (stop-children self)
  (call-next-method)
  (notify-watchers-about-stop self))

;; -------------------------------
;; Async handling
;; -------------------------------

(defclass async-waitor-actor (actor) ())

(defmacro with-waiting-actor (actor message system time-out &rest body)
  (alexandria:with-gensyms (self msg state msgbox waiting-actor)
    `(let ((,msgbox (if ,system
                        (make-instance 'mesgb:message-box/dp
                                       :name (string (gensym "waiter-mb/dp-"))
                                       :dispatcher
                                       (getf (asys:dispatchers ,system) :shared))
                        (make-instance 'mesgb:message-box/bt
                                       :name (string (gensym "waiter-mb/bt-")))))
           (,waiting-actor (make-instance
                            'async-waitor-actor
                            :receive (lambda (,self ,msg ,state)
                                        (unwind-protect
                                             (progn
                                               (funcall ,@body ,msg)
                                               (act-cell:stop ,self)
                                               (cons ,msg ,state))
                                          (act-cell:stop ,self)))
                            :name (string (gensym "Ask-Waiter-")))))
       (setf (act-cell:msgbox ,waiting-actor) ,msgbox)
       (act-cell::submit-message ,actor ,message nil ,waiting-actor ,time-out))))

(defmethod ask ((self actor) message &key (time-out nil))
  (future:make-future
   (lambda (promise-fun)
     (log:debug "Executing future function...")
     (let* ((context (context self))
            (system (if context (ac:system context) nil))
            (timed-out nil)
            (result-received nil))
       (with-waiting-actor self message system time-out
         (lambda (result)
           (setf result-received t)
           (log:info "Result: ~a, timed-out:~a" result timed-out)
           (unless timed-out
             (funcall promise-fun result))))
       (when time-out
         (handler-case
             (utils:with-waitfor (time-out)
               (utils:wait-cond (lambda () result-received) 0.1))
           (bt:timeout (c)
             (log:error "Timeout condition: ~a" c)
             (setf timed-out t)
             ;; fullfil the future
             (funcall promise-fun
                      (cons :handler-error
                            (make-condition 'utils:ask-timeout :wait-time time-out
                                                               :cause c))))))))))

;; -------------------------------
;; eventstream protocol impl
;; -------------------------------

(defmethod subscribe ((actor actor) (subscriber actor) &optional pattern)
  "Convenience. Allows to subscribe to `ev:eventstream` by just providing the actor."
  (ev:subscribe (ac:system (context actor)) subscriber pattern))

(defmethod unsubscribe ((actor actor) (unsubscriber actor))
  "Convenience. Allows to unsubscribe to `ev:eventstream` by just providing the actor."
  (ev:unsubscribe (ac:system (context actor)) unsubscriber))

(defmethod publish ((actor actor) message)
  "Convenience. Allows to publish to `ev:eventstream` by just providing the actor."
  (ev:publish (ac:system (context actor)) message))


;; --------------------------------------
;; actor-context protocol impl (partial)
;; --------------------------------------

(defmethod find-actors ((actor actor) test-fun)
  "`ac:actor-context` protocol implementation."
  (ac:find-actors (context actor) test-fun))

(defmethod find-actor-by-name ((actor actor) name)
  "`ac:actor-context` protocol implementation."
  (ac:find-actor-by-name (context actor) name))

(defmethod all-actors ((actor actor))
  "`ac:actor-context` protocol implementation."
  (ac:all-actors (context actor)))
