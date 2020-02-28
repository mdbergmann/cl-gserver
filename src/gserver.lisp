(defpackage :cl-gserver
  (:use :cl :lparallel :lparallel.queue :log4cl)
  (:export #:set-threadpool-size
           #:set-receive-fun
           #:send))

(in-package :cl-gserver)

(defun init-threadpool (size)
  (setf *kernel* (make-kernel size)))

(defstruct actor-state (running t :type boolean))

(defclass gserver()
  ((name :initarg :name
         :initform (concatenate 'string "Server-" (princ-to-string (random 100000)))
         :accessor name
         :documentation "Well, the name of the actor. If no name is specified a default one is applied.")
   (mailbox :initform (make-channel)
            :accessor mailbox
            :documentation "The channel for submitting calls.")
   (internal-state :initarg :internal-state
                   :initform (make-actor-state)
                   :documentation "The internal state of the actor.")
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
                   nil))))))
      (t (c)
        (log:warn "Error condition was raised on message processing: " c)
        nil))))

(defun handle-call-internal (msg)
  (log:debug "Internal handle-call: " msg)
  nil)


;; dummy test gserver
(defclass mygserver (gserver) ())
(defmethod handle-call ((self mygserver) message)
  (log:debug "handle-call from mygserver: " message)
  (cond
    ((equal message "Foo") 
     (progn
       (sleep 3)
       (log:debug "Message handled: Foo")
       t))
    (t nil)))

;; test receive handler

;;(defun error-receive (msg)
;;  (throw 'foo 1))

;; TODO:
;; OK - do loop while, until 'stop-condition
;; OK - add internal state for if we are running or not. When STOP was sent we should go to stopped state.
;; - how to let 'destroy' a channel for cleanup?
;; - add state
;; - add gserver mgr that can spawn new actors.
;; - add error handling
