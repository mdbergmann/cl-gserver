
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
                    ac:all-actors
                    ac:actor-of))

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
    (push watcher watchers)))

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
       (act-cell::submit-message ,actor ,message nil ,waiting-actor ,time-out)
       ,waiting-actor)))

(defmethod ask ((self actor) message &key (time-out nil))
  (future:make-future
   (lambda (promise-fun)
     (lf:ldebug "Executing future function...")
     (let* ((context (context self))
            (system (if context (ac:system context) nil))
            (timed-out-p nil)
            (result-received-p nil)
            (waiting-actor nil))
       (flet ((handle-timeout (&optional cause)
                (lf:linfo "Timeout condition: ~a" cause)
                (setf timed-out-p t)
                (funcall promise-fun
                         (cons :handler-error
                               (make-condition 'utils:ask-timeout
                                               :wait-time time-out
                                               :cause cause)))
                (tell waiting-actor :stop))
              (handle-error (&optional cause)
                (lf:lwarn "~a" cause)
                (funcall promise-fun
                         (cons :handler-error cause))
                (tell waiting-actor :stop)))
         (setf waiting-actor
               (with-waiting-actor self message system time-out
                 (lambda (result)
                   (setf result-received-p t)
                   (lf:linfo "Result: ~a, timed-out:~a" result timed-out-p)
                   (unless timed-out-p
                     (funcall promise-fun result)))))
         (when time-out
           (when system
             (handler-case
                 (wt:schedule (asys:timeout-timer system)
                              time-out
                              (lambda ()
                                (unless result-received-p
                                  (handle-timeout))))
               (error (c)
                 (handle-error c))))
           (unless system
             (handler-case
                 (utils:with-waitfor (time-out)
                   (utils:wait-cond (lambda () result-received-p) 0.1))
               (bt:timeout (c)
                 (handle-timeout c))))))))))

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

(defmethod find-actors ((actor actor) path &key (test #'string=) (key #'act-cell:name))
  "`ac:actor-context` protocol implementation."
  (ac:find-actors (context actor) path :test test :key key))

(defmethod all-actors ((actor actor))
  "`ac:actor-context` protocol implementation."
  (ac:all-actors (context actor)))

(defmethod actor-of ((actor actor)
                     &key receive
                       (init nil) (destroy nil)
                       (dispatcher :shared) (state nil)
                       (type 'act:actor) (name nil))
  "`ac:actor-context` protocol implementation"
  (ac:actor-of (context actor)
    :receive receive
    :init init
    :destroy destroy
    :dispatcher dispatcher
    :state state
    :type type
    :name name))
