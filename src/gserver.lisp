(defpackage :cl-gserver
  (:use :cl :cl-gserver.utils :cl-gserver.fcomputation :log4cl)
  (:local-nicknames (:mb :cl-gserver.messageb))
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:handle-call
           #:handle-cast
           #:gserver
           #:name
           #:call
           #:async-call
           #:cast
           #:after-init
           #:make-gserver))

(in-package :cl-gserver)

(defstruct gserver-state (running t :type boolean))

(defclass gserver ()
   ((name :initarg :name
          :initform (string (gensym "gs-"))
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

A GServer runs a `message-box' which has it's own thread that operates on a message queue.

To stop a Gserver message handling and thread pool you can send the `:stop' message either via `call' (which will respond with `:stopped') or `cast'.
This is to cleanup thread resources when the Gserver is not needed anymore."))

(defmethod initialize-instance :after ((self gserver) &key)
  :documentation "Initializes the instance."
  (log:debug "Initialize instance: ~a~%" self)

  (with-slots (max-queue-size msgbox) self
    (setf msgbox (make-instance 'mb:message-box-bt :max-queue-size max-queue-size))))

;; -----------------------------------------------
;; public functions
;; -----------------------------------------------

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

(defmacro %with-async-call (gserver message &rest body)
"Macro that makes a `call', but asynchronous. Therefore it spawns a new gserver which waits for the result.
The provided body is the response handler."
  (with-gensyms (self msg state)
    `(make-gserver (string (gensym "gs-"))
                   :cast-fun (lambda (,self ,msg ,state)
                               (unwind-protect
                                    (progn
                                      (funcall ,@body ,msg)
                                      (cl-gserver:cast ,self :stop)
                                      (cons ,msg ,state))
                                 (cl-gserver:cast ,self :stop)))
                   :after-init-fun (lambda (,self ,state)
                                     (declare (ignore ,state))
                                     ;; this will call the `cast' function
                                     ;; that's why it's implemented above
                                     (cl-gserver::submit-message ,gserver ,message nil ,self)))))

(defun async-call (gserver message)
  (make-instance 'fcomputation
                 :exec-fun (lambda (set-computation-value-fun)
                             (log:debug "Executing fcomputation function...")
                             (%with-async-call gserver message
                                               (lambda (result)
                                                 (log:debug "Result: ~a~%" result)
                                                 (funcall set-computation-value-fun result))))))

;; -----------------------------------------------    
;; internal functions
;; -----------------------------------------------

(defun stop-server (gserver)
  (log:debug "Stopping server and message handling!")
  (with-slots (msgbox internal-state) gserver
    (mb:stop msgbox)
    (setf (slot-value internal-state 'running) nil)))

(defun submit-message (gserver message withreply-p sender)
"Submitting a message.
In case of `withreply-p', the `response' is filled because submitting to the message-box is synchronous.
Otherwise submitting is asynchronous and `response' is just `t'.
In case the gserver was stopped it will respond with just `:stopped'."
  (log:debug "Submitting message: " message)
  (log:debug "Withreply: " withreply-p)
  (log:debug "Sender: " sender)

  (with-slots (internal-state) gserver
    (unless (gserver-state-running internal-state)
      (return-from submit-message :stopped)))
  
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
                 (stop-server gserver)
                 :stopped))
    (t (progn
         (when sender
           (progn
             (log:debug "We have a sender. Send the response back: " sender)
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

;; -----------------------------------------------
;; -------- simple gserver -----------------------
;; -----------------------------------------------
(defclass simple-gserver (gserver)
  ((call-fun :initarg :call-fun
             :initform nil
             :documentation "The `call' function specified as slot.")
   (cast-fun :initarg :cast-fun
             :initform nil
             :documentation "The `cast' function specified as slot.")
   (after-init-fun :initarg :after-init-fun
                   :initform nil
                   :documentation "Code to be called after gserver start."))
  (:documentation
   "A simple gserver to be created just with `make-gserver'."))

(defmethod initialize-instance :after ((self simple-gserver) &key)
  (log:debug "Initialize instance: ~a~%" self)
  (after-init self (slot-value self 'state)))

(defmethod after-init ((self simple-gserver) state)
  (with-slots (after-init-fun) self
    (when after-init-fun
      (funcall after-init-fun self state))))

(defmethod handle-call ((self simple-gserver) message current-state)
  (with-slots (call-fun) self
    (when call-fun
      (funcall call-fun self message current-state))))

(defmethod handle-cast ((self simple-gserver) message current-state)
  (with-slots (cast-fun) self
    (when cast-fun
      (funcall cast-fun self message current-state))))

(defun make-gserver (name &key state call-fun cast-fun after-init-fun)
"Makes a new `simple-gserver' which allows you to specify
a `name' for the gserver and also `:state', `call-fun', `cast-fun' and `after-init-fun'."
  (make-instance 'simple-gserver :name name
                                 :state state
                                 :call-fun call-fun
                                 :cast-fun cast-fun
                                 :after-init-fun after-init-fun))
