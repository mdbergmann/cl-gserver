(defpackage :cl-gserver
  (:use :cl :cl-gserver.utils :lparallel :log4cl)
  (:local-nicknames (:mb :cl-gserver.messageb))
  (:export #:init-dispatcher-threadpool
           #:handle-call
           #:handle-cast
           #:gserver
           #:name
           #:call
           #:cast
           #:after-init))

(in-package :cl-gserver)

(defun init-dispatcher-threadpool (size)
  (log:debug "Initializing dispatcher threadpool with " size " threads")
  (setf *kernel* (make-kernel size)))

(defstruct gserver-state (running t :type boolean))

(defclass gserver ()
   ((name :initarg :name
          :initform (mkstr "Server-" (random 100000))
          :accessor name
          :documentation
          "The name of the gserver. If no name is specified a default one is applied.")
    (state :initarg :state
           :initform nil
           :documentation
           "The encapsulated state.")
    (internal-state :initarg :internal-state
                    :initform (make-gserver-state)
                    :documentation
                    "The internal state of the server.")
    (max-queue-size :initarg :max-queue-size
                    :initform nil
                    :documentation
                    "0 or nil for unbounded queue. > 0 for bounded queue. Don't choose < 10.")
    (msgbox :initform nil))
  (:documentation
"GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.
State can be changed by calling into the server via `call' or `cast'.
Where `call' is waiting for a result and `cast' does not.
For each `call' and `cast' handlers must be implemented by subclasses.

A GServer runs it's own thread, actually a lparallel message-kernel with one worker, 
to handle the messages which will eventually update the state.

To stop a Gserver message handling and thread pool you can send the `:stop' message either via `call' (which will respond with `:stopped') or `cast'.
This is to cleanup thread resources when the Gserver is not needed anymore."))

(defmethod initialize-instance :after ((self gserver) &key)
  :documentation "Initializes the instance."
  (log:debug "Initialize instance: ~a~%" self)

  (with-slots (max-queue-size msgbox) self
    (setf msgbox (make-instance 'mb:message-box-bt :max-queue-size max-queue-size))))

;; public functions

(defgeneric after-init (server state)
  (:documentation
"Generic function definition that you may call from `initialize-instance'."))

(defgeneric handle-call (gserver message current-state)
  (:documentation
"Handles calls to the server. Must be implemented by subclasses.
The convention here is to return a `cons' with values to be returned to caller as `car', and the new state as `cdr'.
`handle-call' is executed in the default message dispatcher thread."))

(defgeneric handle-cast (gserver message current-state)
  (:documentation
"Handles casts to the server. Must be implemented by subclasses.
Same convention as for 'handle-call' except that no return is sent to the caller. This function returns immediately."))

(defun call (gserver message)
"Send a message to a gserver instance and wait for a result.
The result can be of different types.
Success result: <returned-state>
qUnhandled result: :unhandled
Error result: (cons :handler-error <error-description-as-string>)"
  (when message
    (let ((result (submit-message gserver message t)))
      (log:debug "Message process result:" result)
      result)))

(defun cast (gserver message)
"Sends a message to a gserver asynchronously. There is no result."
  (when message
    (let ((result (submit-message gserver message nil)))
      (log:debug "Message process result:" result)
      result)))

;; internal functions

(defun stop-server (gserver)
  (log:debug "Stopping server and message handling!")
  (with-slots (msgbox internal-state) gserver
    (mb:stop msgbox)
    (setf internal-state nil)))

(defun submit-message (gserver message withreply-p)
  (let ((response
          (mb:with-submit-handler
              ((slot-value gserver 'msgbox)
               message
               withreply-p)
              (handle-message gserver message withreply-p))))
    (after-submit-message gserver message response)))

(defun after-submit-message (gserver message response)
  (case message
    (:stop (progn
             (stop-server gserver)
             :stopped))
    (t response)))

;; ------------------------------------------------
;; --------- message handling ---------------------
;; ------------------------------------------------
(defun handle-message (gserver message withreply-p)
  "This function is `handler-fun' as submitted to the message-box."
  (log:debug "Handling message: " message)
  (when message
    (handler-case
        (unless (handle-message-internal message)
          (handle-message-user gserver message withreply-p))
      (t (c)
        (log:warn "Error condition was raised on message processing: " c)
        (cons :handler-error c)))))

(defun handle-message-internal (msg)
"A `:stop' message will response with `:stopping' and the user handlers are not called.
Otherwise the result is `nil' to resume user message handling."
  (log:debug "Internal handle-call: " msg)
  (case msg
    (:stop :stopping)
    (t nil)))

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
       (process-handler-result handle-result gserver))
      (t
       (process-not-handled)))))

(defun process-handler-result (handle-result gserver)
  (log:info "Message handled, result: " handle-result)
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
    (setf (slot-value gserver 'state) new-state)
    (slot-value gserver 'state)))

(defun reply-value (cons-result)
  (car cons-result))
