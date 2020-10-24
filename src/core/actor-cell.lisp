(defpackage :cl-gserver.actor-cell
  (:use :cl :cl-gserver.utils)
  (:nicknames :act-cell)
  (:export #:handle-call
           #:handle-cast
           #:before-start
           #:after-stop
           #:actor-cell
           #:cell
           #:name
           #:msgbox
           #:system
           #:state
           #:call
           #:cast
           #:running-p))

(in-package :cl-gserver.actor-cell)

(defstruct actor-cell-state (running t :type boolean))

(defclass actor-cell ()
   ((name :initarg :name
          :initform (string (gensym "actor-"))
          :reader name
          :documentation
          "The name of the gserver. If no name is specified a default one is applied.")
    (state :initarg :state
           :initform nil
           :documentation
           "The encapsulated state.")
    (internal-state :initform (make-actor-cell-state)
                    :documentation
                    "The internal state of the server.")
    (msgbox :initarg :msgbox
            :initform nil
            :accessor msgbox
            :documentation
            "The `message-box'.")
    (system :initform nil
            :accessor system
            :documentation "The system of this actor."))
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

(defmethod print-object ((obj actor-cell) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name state internal-state msgbox system) obj
      (format stream "~a, running: ~a, state: ~a, message-box: ~a"
              name
              (slot-value internal-state 'running)
              state
              msgbox))))

(defmethod initialize-instance :after ((obj actor-cell) &key)
  (log:debug "initialized: ~a" obj)
  (before-start obj (slot-value obj 'state)))

;; -----------------------------------------------
;; public functions
;; -----------------------------------------------

(defgeneric before-start (actor-cell state)
  (:documentation
   "Generic function definition that called from `initialize-instance'."))

(defmethod before-start ((self actor-cell) state)
  "Empty implementation so that we can call it anyway even if there are no other implementations."
  (declare (ignore state))
  nil)

(defgeneric after-stop (actor-cell)
  (:documentation
   "Generic function definition that is called after the actor cell has stopped."))

(defmethod after-stop ((self actor-cell))
  "Empty implementation so that we can call it anyway even if there are no other implementations."
  nil)

(defgeneric handle-call (actor-cell message current-state)
  (:documentation
   "Handles calls to the server. Must be implemented by subclasses.
The convention here is to return a `cons' with values to be returned to caller as `car', and the new state as `cdr'.
`handle-call' is executed in the default message dispatcher thread."))

(defgeneric handle-cast (actor-cell message current-state)
  (:documentation
   "Handles casts to the server. Must be implemented by subclasses.
Same convention as for 'handle-call' except that no return is sent to the caller. This function returns immediately."))

(defun call (actor-cell message)
  "Send a message to a actor-cell instance and wait for a result.
The result can be of different types.
Success result: <returned-state>
Unhandled result: `:unhandled'
Error result: `(cons :handler-error <error-description-as-string>)'"
  (when message
    (let ((result (submit-message actor-cell message t nil)))
      (log:debug "Message process result:" result)
      result)))

(defun cast (actor-cell message)
  "Sends a message to a actor-cell asynchronously. There is no result."
  (when message
    (let ((result (submit-message actor-cell message nil nil)))
      (log:debug "Message process result:" result)
      result)))  

(defun running-p (actor-cell)
  "Returns true if this server is running. `nil' otherwise."
  (with-slots (internal-state) actor-cell
    (slot-value internal-state 'running)))

;; -----------------------------------------------    
;; internal functions
;; -----------------------------------------------

(defun stop (actor-cell)
  (log:debug "Stopping server and message handling!")
  (with-slots (msgbox internal-state) actor-cell
    (mesgb:stop msgbox)
    (setf (slot-value internal-state 'running) nil)
    (after-stop actor-cell)))

(defun submit-message (actor-cell message withreply-p sender)
  "Submitting a message.
In case of `withreply-p', the `response' is filled because submitting to the message-box is synchronous.
Otherwise submitting is asynchronous and `response' is just `t'.
In case the actor-cell was stopped it will respond with just `:stopped'.
In case no messge-box is configured this function respnds with `:no-message-handling'."
  (log:debug "Submitting message: " message)
  (log:debug "Withreply: " withreply-p)
  (log:debug "Sender: " sender)

  (with-slots (internal-state msgbox) actor-cell
    (unless (actor-cell-state-running internal-state)
      (return-from submit-message :stopped))
    (unless msgbox
      (return-from submit-message :no-message-handling)))

  (let ((response
          (mesgb:with-submit-handler
              ((slot-value actor-cell 'msgbox)
               message
               withreply-p)
              (process-response actor-cell
                                (handle-message actor-cell message withreply-p)
                                sender))))
    response))

(defun process-response (actor-cell handle-result sender)
  (log:debug "Processing handle-result: " handle-result)
  (case handle-result
    (:stopping (progn
                 (stop actor-cell)
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

(defun handle-message (actor-cell message withreply-p)
  "This function is submitted as `handler-fun' to message-box"
  (log:debug "Handling message: " message)
  (when message
    (handler-case
        (let ((internal-handle-result (handle-message-internal message)))
          (case internal-handle-result
            (:resume (handle-message-user actor-cell message withreply-p))
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

(defun handle-message-user (actor-cell message withreply-p)
  "This will call the method 'handle-call' with the message."
  (log:debug "User handle message: " message)
  (let* ((current-state (slot-value actor-cell 'state))
         (handle-result
           (if withreply-p
               (progn
                 (log:debug "Calling handle-call on: " actor-cell)
                 (handle-call actor-cell message current-state))
               (progn
                 (log:debug "Calling handle-cast on: " actor-cell)
                 (handle-cast actor-cell message current-state)))))
    (log:debug "Current-state: " (slot-value actor-cell 'state))
    (cond
      (handle-result
       (process-handler-result handle-result actor-cell))
      (t
       (process-not-handled)))))

(defun process-handler-result (handle-result actor-cell)
  (log:debug "Message handled, result: " handle-result)
  (cond
    ((consp handle-result)
     (progn
       (log:debug "Updating state...")
       (update-state actor-cell handle-result)
       (log:debug "Updating state...done")
       (reply-value handle-result)))
    (t
     (progn
       (log:warn "handle-call result is no cons.")
       (cons :handler-error "handle-call result is no cons!")))))

(defun process-not-handled ()
  (log:debug "Message not handled.")
  :unhandled)

(defun update-state (actor-cell cons-result)
  (let ((new-state (cdr cons-result)))
    (setf (slot-value actor-cell 'state) new-state)
    (slot-value actor-cell 'state)))

(defun reply-value (cons-result)
  (car cons-result))
