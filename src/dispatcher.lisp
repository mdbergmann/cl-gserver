
(in-package :sento.dispatcher)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import '(mesgb:message-box/bt
                      mesgb:submit
                      mesgb:handler-unwound-error
                      timeutils:ask-timeout
                      act:actor
                      act:tell
                      act:ask-s
                      act-cell:name
                      act-cell:msgbox
                      act-cell:running-p)))

(defclass dispatcher-base ()
  ((context :initform nil
            :initarg :context)
   (identifier :initform nil
               :initarg :identifier
               :reader identifier
               :documentation "Returns the identifier of the dispatcher.")
   (throughput :initform *default-throughput*
               :initarg :throughput
               :reader throughput
               :documentation
               "The number of queued messages a message-box using this dispatcher
handles in one run on a worker before it yields the worker."))
  (:documentation
   "A `dispatcher` contains a pool of `actors` that operate as workers where work is dispatched to.
However, the workers are created in the given `ac:actor-context`."))

;; ---------------------------------
;; Shared dispatcher
;; ---------------------------------

(defun make-dispatcher (actor-context identifier &rest config)
  "Default constructor.
This creates a `disp:shared-dispatcher` with the given dispatcher config, see `asys:*default-config*`.
Each worker is based on a `:pinned` actor meaning that it has its own thread.
Specify an `ac:actor-context` where actors needed in the dispatcher are created in."
  (make-instance 'shared-dispatcher
                 :context actor-context
                 :identifier identifier
                 :num-workers (getf config :workers 2)
                 :strategy (getf config :strategy :random)
                 :throughput (getf config :throughput *default-throughput*)))

(defclass shared-dispatcher (dispatcher-base)
  ((router :initform nil))
  (:documentation
   "A shared dispatcher.
Internally it uses a `router:router` to drive the `dispatch-worker`s.
The default strategy of choosing a worker is `:random`.

A `shared-dispatcher` is automatically setup by an `asys:actor-system`."))

(defmethod initialize-instance :after ((self shared-dispatcher) &key (num-workers 1) (strategy :random))
  (with-slots (router context identifier) self
    (setf router (router:make-router :strategy strategy))
    (loop :for n :from 1 :to num-workers
          :do (router:add-routee router (make-dispatcher-worker n context identifier)))))

(defmethod print-object ((obj shared-dispatcher) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (router identifier throughput) obj
      (format stream "ident: ~a, workers: ~a, strategy: ~a, throughput: ~a"
              identifier
              (length (router:routees router))
              (router:strategy-fun router)
              throughput))))

(defmethod workers ((self shared-dispatcher))
  (with-slots (router) self
    (router:routees router)))

(defmethod stop ((self shared-dispatcher))
  (with-slots (router) self
    (router:stop router)))

(defmethod dispatch ((self shared-dispatcher) dispatch-exec-fun-args)
  (with-slots (router) self
    (router:ask-s router dispatch-exec-fun-args)))

(defmethod dispatch-async ((self shared-dispatcher) dispatch-exec-fun-args)
  (with-slots (router) self
    (router:tell router dispatch-exec-fun-args)))


;; ---------------------------------
;; the worker
;; ---------------------------------

(defclass dispatch-worker (actor) ()
  (:documentation
   "Specialized `actor` used as `worker` in the message `dispatcher`.
The message a worker receives is the dispatched function call as a list:
the function followed by its arguments.
`tell` and `ask-s` on a worker submit that list straight to the worker's
message-box instead of going through the `act-cell` message handling.
A worker has no state, no behavior and no sender to bind, so that layer
would only add per-message overhead on every message of every actor that
runs on the dispatcher."))

(defun make-dispatcher-worker (num actor-context dispatcher-ident)
  "Constructor for creating a worker.
`num` only has the purpose to give the worker a name which includes a number.
`dispatcher-ident is the dispatcher identifier."
  (ac:actor-of actor-context
    :receive #'execute-dispatched
    :type 'dispatch-worker
    :name (format nil "dispatch(~a)-worker-~a" dispatcher-ident num)
    :dispatcher :pinned))

(defun execute-dispatched (message)
  "Applies the dispatched function, the `car` of `message`, to the rest of
`message`. A condition signaled by the function is logged and returned as
`(cons :handler-error condition)`, as an actor message handler would, so that
a synchronous `dispatch` gets a result instead of unwinding the worker thread.
Runs without the `act-cell` dynamic bindings, so it must not rely on `*self*`."
  (handler-case
      (apply (car message) (cdr message))
    (serious-condition (c)
      (log:error "dispatched function ~a signaled: ~a" message c)
      (cons :handler-error c))))

(defmethod tell ((self dispatch-worker) message &optional sender)
  "Submits the dispatched function call `message` to the worker's message-box.
Returns `T` when submitted, `:stopped` when the worker is stopped."
  (declare (ignore sender))
  (if (running-p self)
      (submit (msgbox self) message nil nil '(execute-dispatched))
      :stopped))

(defmethod ask-s ((self dispatch-worker) message &key time-out)
  "Submits the dispatched function call `message` to the worker's message-box
and waits for its result. Returns `:stopped` when the worker is stopped and
`(cons :handler-error condition)` on a time-out or when the function unwound
without a result, as `act-cell:call` does."
  (if (running-p self)
      (handler-case
          (submit (msgbox self) message t time-out '(execute-dispatched))
        (ask-timeout (c)
          (log:warn "~a: dispatch time-out: ~a" (name self) c)
          (cons :handler-error c))
        (handler-unwound-error (c)
          (log:warn "~a: dispatched function unwound: ~a" (name self) c)
          (cons :handler-error c)))
      :stopped))
