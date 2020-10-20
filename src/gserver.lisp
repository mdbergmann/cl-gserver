(defpackage :cl-gserver.gserver
  (:use :cl :cl-gserver.utils :cl-gserver.future)
  (:nicknames :gs)
  (:local-nicknames (:mb :cl-gserver.messageb))
  (:export #:handle-call
           #:handle-cast
           #:gserver
           #:name
           #:state
           #:msgbox
           #:call
           #:cast
           #:running-p))

(in-package :cl-gserver.gserver)

(defstruct gserver-state (running t :type boolean))

(defclass gserver ()
   ((name :initarg :name
          :initform (string (gensym "gs-"))
          :reader name
          :documentation
          "The name of the gserver. If no name is specified a default one is applied.")
    (state :initarg :state
           :initform nil
           :reader state
           :documentation
           "The encapsulated state.")
    (internal-state :initarg :internal-state
                    :initform (make-gserver-state)
                    :documentation
                    "The internal state of the server.")
    (msgbox :initform nil
            :documentation
            "The `message-box'.")
    (system :initform nil
            :reader system
            :documentation
            "The system where this actor is part of."))
  (:documentation
   "GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.
State can be changed by calling into the server via `call' or `cast'.
Where `call' is waiting for a result and `cast' does not.
For each `call' and `cast' handlers must be implemented by subclasses.

A GServer runs a `message-box' that processes all the received messages.
When the GServer was created ad-hoc (out of the `system'), then it will create
a `message-box' with it's own thread.
If the GServer is created through the `system', then it will use the system-wide thread-pool
to process the `message-box'.

To stop a Gserver message handling and you can tell the `:stop' message 
either via `call' (which will respond with `:stopped') or `cast'.
This is to cleanup thread resources when the Gserver is not needed anymore."))

(defmethod print-object ((obj gserver) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name state internal-state msgbox system) obj
      (format stream "~a, system: ~a, running: ~a, state: ~a, message-box: ~a"
              name
              (not (null system))
              (slot-value internal-state 'running)
              state
              msgbox))))

;; -----------------------------------------------
;; public functions
;; -----------------------------------------------

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
Unhandled result: `:unhandled'
Error result: `(cons :handler-error <error-description-as-string>)'"
  (when message
    (let ((result (submit-message gserver message t nil)))
      (log:debug "Message process result:" result)
      result)))

(defun cast (gserver message)
  "Sends a message to a gserver asynchronously. There is no result."
  (when message
    (let ((result (submit-message gserver message nil nil)))
      (log:debug "Message process result:" result)
      result)))  

(defun running-p (gserver)
  "Returns true if this server is running. `nil' otherwise."
  (with-slots (internal-state) gserver
    (slot-value internal-state 'running)))

;; -----------------------------------------------    
;; internal functions
;; -----------------------------------------------

(defun stop (gserver)
  (log:debug "Stopping server and message handling!")
  (with-slots (msgbox internal-state) gserver
    (mb:stop msgbox)
    (setf (slot-value internal-state 'running) nil)))

(defun submit-message (gserver message withreply-p sender)
  "Submitting a message.
In case of `withreply-p', the `response' is filled because submitting to the message-box is synchronous.
Otherwise submitting is asynchronous and `response' is just `t'.
In case the gserver was stopped it will respond with just `:stopped'.
In case no messge-box is configured this function respnds with `:no-message-handling'."
  (log:debug "Submitting message: " message)
  (log:debug "Withreply: " withreply-p)
  (log:debug "Sender: " sender)

  (with-slots (internal-state msgbox) gserver
    (unless (gserver-state-running internal-state)
      (return-from submit-message :stopped))
    (unless msgbox
      (return-from submit-message :no-message-handling)))

  (let ((response
          (mb:with-submit-handler
              ((slot-value gserver 'msgbox)
               message
               withreply-p)
              (process-response gserver
                                (handle-message gserver message withreply-p)
                                sender))))
    response))

(defun process-response (gserver handle-result sender)
  (log:debug "Processing handle-result: " handle-result)
  (case handle-result
    (:stopping (progn
                 (stop gserver)
                 :stopped))
    (t (progn
         (when sender
           (progn
             (log:debug "We have a teller. Send the response back: " sender)
             (cast sender handle-result)))
         handle-result))))

;; ------------------------------------------------
;; message handling ---------------------
;; ------------------------------------------------

(defun handle-message (gserver message withreply-p)
  "This function is submitted as `handler-fun' to message-box"
  (log:debug "Handling message: " message)
  (when message
    (handler-case
        (let ((internal-handle-result (handle-message-internal message)))
          (case internal-handle-result
            (:resume (handle-message-user gserver message withreply-p))
            (t internal-handle-result)))
      (t (c)
        (log:warn "Error condition was raised on message processing: " c)
        (cons :handler-error c)))))

(defun handle-message-internal (msg)
  "A `:stop' message will response with `:stopping' and the user handlers are not called.
Otherwise the result is `:resume' to resume user message handling."
  (log:debug "Internal handle-call: " msg)
  (case msg
    (:stop :stopping)
    (t :resume)))

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
  (log:debug "Message handled, result: " handle-result)
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
