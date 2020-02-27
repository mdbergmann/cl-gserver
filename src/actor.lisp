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
   (mailbox :initform (make-channel)
            :accessor mailbox)
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
  :documentation "Not sure yet what this does.")

;; public functions

(defun set-receive-fun (actor new-receive-fun)
  "Set the receive handler function on an actor."
  (when new-receive-fun
    (with-slots (receive-fun) actor
      (setf receive-fun new-receive-fun))))

(defun send (actor message)
  "Send a message to an actor"
  (when message
    (log:debug "pushing ~a to mailbox" message)
    (let ((result (submit-to-mailbox actor message)))
      (log:debug "Message process result:" result))))

(defun stop (actor)
  "Stops the message processing thread."
  (send actor 'stop))

;; internal functions

(defun submit-to-mailbox (actor message)
  "Pushes the message to the mailbox channel"
  (let ((*task-category* (concatenate 'string (name actor) "-task")))
    (submit-task (mailbox actor) (lambda () (process-message actor (unwrap-message message))))
    (receive-result (mailbox actor))))

(defun process-message (actor message)
  (log:debug "Handling message: " message)
  (when message
    (handler-case (progn
                    ;; First process internally
                    (when (not (internal-receive-fun message))
                      ;; then externally
                      (let ((user-receive-fun-result (funcall (receive-fun actor) message)))
                        (cond
                          (user-receive-fun-result (log:debug "Message handled by user."))
                          (t (log:debug "Message not handled."))))))
      (t (c) (log:warn "Error condition was raised on message processing: " c)
        nil))))

(defun internal-receive-fun (msg)
  (log:debug "Internal receive: " msg)
  nil)

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
;; - add actor mgr that can spawn new actors.
;; - add state
;; - add error handling
