(defpackage :cl-gserver
  (:use :cl :cl-gserver.utils :lparallel :stmx :log4cl)
  (:export #:init-threadpool
           #:handle-call
           #:handle-cast
           #:gserver
           #:name
           #:call
           #:cast))

(in-package :cl-gserver)

(defun init-threadpool (size)
  (log:debug "Initializing threadpool with " size " threads")
  (setf *kernel* (make-kernel size)))

(defstruct gserver-state (running t :type boolean))

(defclass gserver()
   ((name :initarg :name
          :initform (mkstr "Server-" (random 100000))
          :accessor name
          :documentation "Well, the name of the gserver. If no name is specified a default one is applied.")
   (state :initarg :state
          :initform nil
          :documentation "The encapsulated state.")
   (internal-state :initarg :internal-state
                   :initform (make-gserver-state)
                   :documentation "The internal state of the server."))
  (:documentation
"GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.
State can be changed by calling into the server via 'call' or 'cast'.
Where 'call' is waiting for a result and 'cast' does not.
For each 'call' and 'cast' handlers must be implemented by subclasses.

The difference to the Erlang GenServer is that GServer doesn't have it's own process. Instead it is more a facade over a combination of the lparallel workers and stmx transactional memory to make sure the state is handled properly in an asynchronous manner."))

(defmethod initialize-instance :after ((self gserver) &key)
  :documentation "Not sure yet what this does.")

;; public functions

(defgeneric handle-call (gserver message current-state)
  (:documentation
"Handles calls to the server. Must be implemented by subclasses.
The convention here is to return a cons with values to be returned to caller as car, and the new state as cdr."))

(defgeneric handle-cast (gserver message current-state)
  (:documentation
"Handles casts to the server. Must be implemented by subclasses.
Same convention as for 'handle-call' except that no return is sent to the caller. This function returns immediately."))

(defun call (gserver message)
"Send a message to a gserver instance and wait for a result.
The result is a of different types.
Success result: <returned-state>
Unhandled result: :unhandled
Error result: (cons :handler-error <error-description-as-string>)
"
  (when message
    (log:debug "pushing ~a to channel" message)
    (let ((result (submit-message-sync gserver message)))
      (log:debug "Message process result:" result)
      result)))

(defun cast (gserver message)
"Sends a message to a gserver asynchronously.
No result."
  (when message
    (log:debug "casting message: " message)
    (submit-message-async gserver message)))

;; internal functions

(defun submit-message-sync (gserver message)
  (let ((*task-category* (mkstr (name gserver) "-task"))
        (channel (make-channel)))
    (submit-task channel (lambda () (handle-message gserver message nil)))
    (receive-result channel)))

(defun submit-message-async (gserver message)
  (future (handle-message gserver message t)))

(defun handle-message (gserver message async-p)
  (log:debug "Handling message: " message)
  (when message
    (handler-case
        (unless (handle-message-internal message)
          (handle-message-user gserver message async-p))
      (t (c)
        (log:warn "Error condition was raised on message processing: " c)
        (cons :handler-error c)))))

(defun handle-message-internal (msg)
  "Returns nil in order to make user handler being invoked."
  (log:debug "Internal handle-call: " msg)
  nil)

(defun handle-message-user (gserver message async-p)
  "This will call the method 'handle-call' with the message."  
  (let* ((current-state (slot-value gserver 'state))
         (handle-result
           (if async-p
               (handle-cast gserver message current-state)
               (handle-call gserver message current-state))))
    (log:debug "Current-state: " (slot-value gserver 'state))
    (cond
      (handle-result
       (process-handle-result handle-result gserver))
      (t
       (process-not-handled)))))

(defun process-handle-result (handle-result gserver)
  (log:debug "Message handled by handle-call. result: " handle-result)
  (cond
    ((consp handle-result)
     (progn
       (update-state gserver handle-result)
       (reply-value handle-result)))
    (t
     (progn
       (log:info "handle-call result is no cons.")
       (cons :handler-error "handle-call result is no cons!")))))

(defun process-not-handled ()
  (log:debug "Message not handled.")
  :unhandled)

(defun update-state (gserver cons-result)
  (let ((new-state (cdr cons-result)))
    (log:info "Updating state to: " new-state)
;    (atomic
     (setf (slot-value gserver 'state) new-state)))

(defun reply-value (cons-result)
  (car cons-result))


;;(defun error-receive (msg)
;;  (throw 'foo 1))

;; TODO:
;; OK - do loop while, until 'stop-condition
;; OK - add internal state for if we are running or not. When STOP was sent we should go to stopped state.
;; OK - return error cons
;; OK - add state
;; OK - add cast, fire-and-forget
;; OK - add error handling
;; - implement stmx to wrap updating the state
;; => - add macro to conveniently create gserver
;; - add gserver mgr that can spawn new actors.
