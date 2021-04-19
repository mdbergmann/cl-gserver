
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
                    ev:publish))

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
             "Behavior function applied via `become` and reverted via `unbecome`
`behavior' function takes the same parameters as `receive`.")
   (context :initform nil
            :accessor context)
   (watchers :initform '()
             :reader watchers
             :documentation "List of watchers of this actor."))
  (:documentation
   "This is the `actor` class.
The `actor` does its message handling using the `receive` function.
There is asynchronous `tell` (no response) and synchronous `ask-s` and asynchronous `ask` (with response).
To stop an actors message processing in order to cleanup resouces you should tell (either `tell` or `ask-s`)
the `:stop` message. It will respond with `:stopped` (in case of `ask(-s)`)."))

(defmethod make-actor (receive &key name state (type 'actor))
  (make-instance type
                 :name name
                 :state state
                 :receive receive))

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
  (ev:subscribe (ac:system (context actor)) subscriber pattern))

(defmethod unsubscribe ((actor actor) (unsubscriber actor))
  (ev:unsubscribe (ac:system (context actor)) unsubscriber))

(defmethod publish ((actor actor) message)
  (ev:publish (ac:system (context actor)) message))
