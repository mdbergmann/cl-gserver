(defpackage :cl-gserver
  (:use :cl :cl-gserver.utils :lparallel :lparallel.queue :log4cl)
  (:export #:init-threadpool
           #:handle-call
           #:gserver
           #:name
           #:call))

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

(defgeneric handle-call (gserver message)
  (:documentation
"Handles calls to the server. Must be implemented by subclasses.
A result can be returned which is forwarded to the caller."))

(defun call (gserver message)
  "Send a message to a gserver instance."
  (when message
    (log:debug "pushing ~a to mailbox" message)
    (let ((result (submit-to-mailbox gserver message)))
      (log:debug "Message process result:" result)
      result)))

;; internal functions

(defun submit-to-mailbox (gserver message)
  "Pushes the message to the mailbox channel"
  (let ((*task-category* (concatenate 'string (name gserver) "-task")))
    (submit-task (mailbox gserver) (lambda () (process-message gserver message)))
    (receive-result (mailbox gserver))))

(defun process-message (gserver message)
  (log:debug "Handling message: " message)
  (when message
    (handler-case
        (progn
          ;; First process internally
          (unless (handle-call-internal message)
            ;; then externally
            (let ((handle-call-result (handle-call gserver message)))
              (cond
                (handle-call-result
                 (progn
                   (log:debug "Message handled by handle-call.")
                   handle-call-result))
                (t
                 (progn
                   (log:debug "Message not handled.")
                   (cons :unhandled "")))))))
      (t (c)
        (log:warn "Error condition was raised on message processing: " c)
        (cons :handler-error c)))))

(defun handle-call-internal (msg)
  (log:debug "Internal handle-call: " msg)
  nil)


;;(defun error-receive (msg)
;;  (throw 'foo 1))

;; TODO:
;; OK - do loop while, until 'stop-condition
;; OK - add internal state for if we are running or not. When STOP was sent we should go to stopped state.
;; => - return error cons 
;; - add state
;; - how to let 'destroy' a channel for cleanup?
;; - add gserver mgr that can spawn new actors.
;; - add error handling
