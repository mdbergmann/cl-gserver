(defpackage :cl-actors2
  (:use :cl :lparallel :lparallel.queue :log4cl)
  (:export #:set-threadpool-size
           #:set-receive-fun
           #:send
           #:stop)
  )

(in-package :cl-actors2)

(defun set-threadpool-size (nthreads)
  (setf *kernel* (make-kernel nthreads)))

(defstruct actor-state (running t :type boolean))

(defclass actor()
  ((name :initarg :name
         :initform (concatenate 'string "Actor-" (princ-to-string (random 100000)))
         :accessor name
         :documentation "Well, the name of the actor. If no name is specified a default one is applied.")
   (mailbox :initform (make-queue))
   (receive-fun :initarg :receive-fun
                :initform (error ":receive-fun must be specified!")
                :accessor receive-fun
                :documentation
                "This is the message handler function. It takes one parameter that is the received message.")
   (internal-state :initarg :internal-state
                   :initform (make-actor-state)
                   :documentation "The internal state of the actor.")
   ))

(defmethod initialize-instance :after ((self actor) &key)
  :documentation "This automatically starts the message consumer loop."
  (start-mailbox-loop self))

(defmethod start-mailbox-loop ((self actor))
  (let ((channel (make-channel)))
    (log:debug "Starting looper on channel...")
    (submit-task channel (lambda ()
                           (handler-case
                               (loop-process-mailbox self)
                             (t (c)
                               (log:debug "Unknown condition received: " c)))))))

(defmethod loop-process-mailbox ((self actor))
  "Loops over message queue endlessly."
  (with-slots (mailbox internal-state) self
    (with-slots (running) internal-state
      (iter:iter (iter:while running)
        (log:debug "Checking for message in queue...")
        (let ((message (pop-queue mailbox)))
          (handler-case
              (process-message self message)
            (stop-condition ()
              (log:debug "Stop condition received. Stopping mailbox processing.")
              (setf running nil))
            (t (c)
              (log:warn "Caught error on message processing: " c)
              (log:warn "Message will not be handled again: " message))))))))

(defmethod process-message ((self actor) &rest message)
  (log:debug "Handling message: " message)
  (when message
    (let ((msg (unwrap-message message)))
      ; First process internally
      (when (not (internal-receive-fun msg))
        (with-slots (receive-fun) self
          ; then externally
          (let ((user-receive-fun-result (funcall receive-fun msg)))
            (cond
              (user-receive-fun-result (log:debug "Message handled by user."))
              (t (log:debug "Message not handled.")))))))))

(defun internal-receive-fun (msg)
  (cond
    ((eq msg 'stop) (progn
                      (log:debug "STOP message received.")
                      (error 'stop-condition)))))

;; public methods

(defmethod set-receive-fun ((self actor) &rest new-receive-fun)
  "Set the receive handler function on an actor."
  (when new-receive-fun
    (with-slots (receive-fun) self
      (setf receive-fun (car new-receive-fun)))))

(defmethod send ((self actor) &rest message)
  "Send a message to an actor"
  (with-slots (internal-state mailbox) self
    (with-slots (running) internal-state
      (if (not running) (log:info "Actor is stopped!"))
      (when (and message running)
        (log:debug "pushing ~a to queue" message)
        (push-queue (unwrap-message message) mailbox)))))

(defmethod stop ((self actor))
  "Stops the message processing thread."
  (send self 'stop))

;; conditions

(define-condition stop-condition (error)
  ()
  (:documentation
   "Stop condition may be raised in message processing to stop the processing thread and the actor."))
  
;; utility functions

(defun unwrap-message (message)
  "If message is a list it takes car otherwise just the message."
  (if (listp message) (car message) message))

;; test receive handler
(defun test-receive (msg)
  (log:debug "received: " msg)
  (cond
    ((equal msg "Foo") 
     (progn
       (log:debug "Message handled: Foo")
       t))
    (t nil)))

(defun error-receive (msg)
  (throw 'foo 1))

;; TODO:
;; OK - do loop while, until 'stop-condition
;; OK - add internal state for if we are running or not. When STOP was sent we should go to stopped state.
;; => - actor can be started again and state can be set again to running.
;; - add state
;; - add error handling
