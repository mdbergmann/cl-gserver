(defpackage :cl-gserver
  (:use :cl :cl-gserver.utils :lparallel :log4cl)
  (:export #:init-dispatcher-threadpool
           #:handle-call
           #:handle-cast
           #:gserver
           #:name
           #:call
           #:cast))

(in-package :cl-gserver)

(defun init-dispatcher-threadpool (size)
  (log:debug "Initializing dispatcher threadpool with " size " threads")
  (setf *kernel* (make-kernel size)))

(defstruct gserver-state (running t :type boolean))

(defclass gserver()
   ((name :initarg :name
          :initform (mkstr "Server-" (random 100000))
          :accessor name
          :documentation
          "The name of the gserver. If no name is specified a default one is applied.")
    (state :initarg :state
           :initform nil
           :documentation
           "The encapsulated state.")
    (state-kernel :initform nil
                  :documentation
                  "The state-kernel with 1 worker for updating the state.")
    (state-channel :initform nil
                   :documentation
                   "The state-channel for the state-kernel. Since we only have 1 worker here it is safe to make an instance channel regarding FIFO.")
    (dispatch-kernel :initform nil
                     :documentation
                     "An optional dispatcher kernel. If none is defined an external lparallel global `*kernel*' is required.")
    (dispatch-kernel-workers :initarg :dispatch-workers
                             :initform 0
                             :documentation
                             "Number of dispatch workers. 0 default, which does spawnm a separate intwernal kernel but an external `*kernel*' is used.")
    (internal-state :initarg :internal-state
                    :initform (make-gserver-state)
                    :documentation
                    "The internal state of the server."))
  (:documentation
"GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.
State can be changed by calling into the server via `call' or `cast'.
Where `call' is waiting for a result and `cast' does not.
For each `call' and `cast' handlers must be implemented by subclasses.

A GServer runs it's own thread, actually a lparallel state-kernel with one worker, to update the state.
The handlers `handle-call' and `handle-cast' are using a default lparallel global kernel.
But the state update of the gserver is synchronized to a single worker queue.
Use `init-dispatcher-threadpool' with > 0 workers."))

(defmethod initialize-instance :after ((self gserver) &key)
  :documentation "Initializes the instance."

  (with-slots (state-kernel
               state-channel
               dispatch-kernel
               dispatch-kernel-workers
               name) self
     (let ((*kernel* (make-kernel 1 :name (mkstr "state-kernel-" name))))
       (setf state-kernel *kernel*)
       (setf state-channel (make-channel)))
    
    (if (> dispatch-kernel-workers 0)
        (progn
          (log:info "Making dispatch kernel with ~a workers" dispatch-kernel-workers)
          (setf dispatch-kernel (make-kernel
                                 dispatch-kernel-workers
                                 :name (mkstr "dispatch-kernel-" name))))
        (progn
          (log:info "Using global *kernel* as distapcher.")
          (setf dispatch-kernel (check-kernel))))))

;; public functions

(defgeneric handle-call (gserver message current-state)
  (:documentation
"Handles calls to the server. Must be implemented by subclasses.
The convention here is to return a `cons' with values to be returned to caller as `car', and the new state as `cdr'.
`handle-call' is executed in the default dispatcher threadpool. Should the threadpool have only 1 worker a long running task will block the handling of other messages.
So make sure the threadpool is sufficiently large to do what you intent to."))

(defgeneric handle-cast (gserver message current-state)
  (:documentation
"Handles casts to the server. Must be implemented by subclasses.
Same convention as for 'handle-call' except that no return is sent to the caller. This function returns immediately."))

(defun call (gserver message)
"Send a message to a gserver instance and wait for a result.
The result can be of different types.
Success result: <returned-state>
Unhandled result: :unhandled
Error result: (cons :handler-error <error-description-as-string>)
"
  (when message
    (let ((result (submit-message gserver message t)))
      (log:debug "Message process result:" result)
      result)))

(defun cast (gserver message)
"Sends a message to a gserver asynchronously.
No result."
  (when message
    (let ((result (submit-message gserver message nil)))
      (log:debug "Message process result:" result)
      result)))

;; internal functions

(defun submit-message (gserver message withreply-p)
  (let* ((*task-category* (mkstr (name gserver) "-task"))
         (*kernel* (slot-value gserver 'dispatch-kernel))
         (channel (make-channel))); make separate channel so that we get the result for the submit.
    (log:debug "Channel: " channel)
    (log:debug "Kernel:" *kernel*)
    (log:debug "Pushing ~a to channel" message)
    (submit-task channel (lambda ()
                           (log:debug "Foo")
                           (handle-message gserver message withreply-p)))
    (if withreply-p
        (receive-result channel)
        (progn
          (future (receive-result channel))
          t)))

(defun handle-message (gserver message withreply-p)
  (log:debug "Handling message: " message)
  (when message
    (handler-case
        (unless (handle-message-internal message)
          (handle-message-user gserver message withreply-p))
      (t (c)
        (log:warn "Error condition was raised on message processing: " c)
        (cons :handler-error c)))))

(defun handle-message-internal (msg)
  "Returns nil in order to make user handler being invoked."
  (log:debug "Internal handle-call: " msg)
  nil)

(defun handle-message-user (gserver message withreply-p)
  "This will call the method 'handle-call' with the message."
  (log:debug "User handle message: " message)
  (let* ((current-state (slot-value gserver 'state))
         (handle-result
           (if withreply-p
               (progn
                 (log:debug "Calling handle-call on: " gserver)
                 (handle-call gserver message current-state))
               (progn
                 (log:debug "Calling handle-cast on: " gserver)
                 (handle-cast gserver message current-state)))))
    (log:debug "Current-state: " (slot-value gserver 'state))
    (cond
      (handle-result
       (process-handle-result handle-result gserver))
      (t
       (process-not-handled)))))

(defun process-handle-result (handle-result gserver)
  (log:info "Message handled by handle-call. result: " handle-result)
  (cond
    ((consp handle-result)
     (progn
       (log:debug "Updating state...")
       (update-state gserver handle-result)
       (log:debug "Updating state...done")
       (reply-value handle-result)))
    (t
     (progn
       (log:warn "handle-call result is no cons.")
       (cons :handler-error "handle-call result is no cons!")))))

(defun process-not-handled ()
  (log:debug "Message not handled.")
  :unhandled)

(defun update-state (gserver cons-result)
  (let ((new-state (cdr cons-result)))
    (let ((channel (slot-value gserver 'state-channel)))
      (submit-task channel
                   (lambda ()
                     (setf (slot-value gserver 'state) new-state)
                     (slot-value gserver 'state)))
      (receive-result channel))))

(defun reply-value (cons-result)
  (car cons-result))


;; TODO:
;; OK - do loop while, until 'stop-condition
;; OK - add internal state for if we are running or not. When STOP was sent we should go to stopped state.
;; OK - return error cons
;; OK - add state
;; OK - add cast, fire-and-forget
;; OK - add error handling
;; - add macro to conveniently create gserver
;; - option to make another state-kernel for cast, call handlers other than *kernel*.
;; - add shutdown of gserver
;; - add gserver mgr that can spawn new actors.
