+(defpackage :cl-gserver
  (:use :cl :cl-gserver.utils :lparallel :lparallel.queue :log4cl)
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
   (mailbox :initform (make-channel)
            :accessor mailbox
            :documentation "The channel for submitting calls.")
   (internal-state :initarg :internal-state
                   :initform (make-gserver-state)
                   :documentation "The internal state of the server.")
   ))

(defmethod initialize-instance :after ((self gserver) &key)
  :documentation "Not sure yet what this does.")

;; public functions

(defgeneric handle-call (gserver message current-state)
  (:documentation
"Handles calls to the server. Must be implemented by subclasses.
The convention here is to return a cons with values to be returned to caller as car,
and the new state as cdr."))

(defgeneric handle-cast (gserver message current-state)
  (:documentation
"Handles casts to the server. Must be implemented by subclasses.
Same convention as for 'handle-call' except that no return is sent to the caller."))

(defun call (gserver message)
"Send a message to a gserver instance and wait for a result.
The result is a of different types.
Success result: <returned-state>
Unhandled result: :unhandled
Error result: (cons :handler-error <error-description-as-string>)
"
  (when message
    (log:debug "pushing ~a to mailbox" message)
    (let ((result (submit-message-synchronous gserver message)))
      (log:debug "Message process result:" result)
      result)))

(defun cast (gserver message)
"Sends a message to a gserver asynchronously.
No result."
  (when message
    (log:debug "casting message: " message)
    (submit-message-asynchronous gserver message)))

;; internal functions

(defun submit-message-synchronous (gserver message)
  "Pushes the message to the mailbox channel"
  (let ((*task-category* (mkstr (name gserver) "-task"))
        (mailbox (mailbox gserver)))
    (submit-task mailbox (lambda () (handle-message gserver message nil)))
    (receive-result mailbox)))

(defun submit-message-asynchronous (gserver message)
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
  (log:info "Updating state to: " (cdr cons-result))
  (setf (slot-value gserver 'state) (cdr cons-result)))

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
;; - future just uses any worker to update the state, not sure if good
;; - add macro to conveniently create gserver
;; - how to let 'destroy' a channel for cleanup?
;; - add gserver mgr that can spawn new actors.
;; - add error handling
