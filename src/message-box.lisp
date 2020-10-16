(defpackage :cl-gserver.messageb
  (:use :cl :cl-gserver.utils :cl-gserver.queue)
  (:import-from :dispatcher-api
                #:dispatch
                #:dispatch-async)
  (:export #:message-box-dp #:message-box-bt
           #:submit
           #:with-submit-handler
           #:stop))

(in-package :cl-gserver.messageb)

(defclass message-box-base ()
  ((name :initform (string (gensym "mb-")))
   (processed-messages :initform 0)
   (max-queue-size :initform 0 :initarg :max-queue-size
                   :documentation
"0 or nil will make an unbounded queue. 
A value > 0 will make a bounded queue. 
Don't make it too small. A queue size of 1000 might be a good choice.")))

(defgeneric submit (message-box-base message withreply-p handler-fun)
  (:documentation "Submit a message to the mailbox to be queued and handled."))

(defgeneric stop (message-box-base)
  (:documentation "Stops the message processing."))

(defmethod stop ((self message-box-base))
  (with-slots (processed-messages) self
    (log:info "Processed messages: " processed-messages)))

(defmethod print-object ((obj message-box-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name processed-messages max-queue-size) obj
      (format stream "~a, processed messages: ~a, max-queue-size: ~a"
              name
              processed-messages
              max-queue-size))))

;; ----------------------------------------
;; ------------- Generic ------------------
;; ----------------------------------------

(defmacro with-submit-handler ((msgbox message withreply-p) &rest body)
  "Macro to let the caller specify a message handler function."
  `(submit ,msgbox ,message ,withreply-p (lambda (message) ,@body)))

;; ----------------------------------------
;; ------------- Bordeaux ----------------
;; ----------------------------------------

(defstruct message-item
  (message nil)
  (withreply-p nil :type boolean)
  (withreply-lock nil)
  (withreply-cvar nil)
  (handler-fun nil :type function))

(defclass message-box-bt (message-box-base)
  ((queue-thread :initform nil
                 :documentation
                 "The thread that pops queue items.")
   (queue :initform nil
          :documentation
          "Which type of queue will be used depends on the `max-queue-size' setting.")
   (should-run :initform t
               :documentation
               "Flag that indicates whether the message processing should commence."))
  (:documentation
"Bordeaux-Threads based message-box with a single thread operating on a message queue.
This is used when the gserver is created outside of the `system'.
There is a limit on the maximum number of gservers/actors/agents that can be created with
this kind of queue because each message-box requires exactly one thread."))

(defmethod initialize-instance :after ((self message-box-bt) &key)
  (log:debug "Initialize instance: ~a~%" self)
  
  (with-slots (name queue queue-thread max-queue-size) self
    (log:debug "Requested max-queue-size: " max-queue-size)
    (setf queue
          (case max-queue-size
            ((0 nil) (make-instance 'queue-unbounded))
            (t (make-instance 'queue-bounded :max-items max-queue-size))))
    (log:info "Using queue: " queue)
    (setf queue-thread (bt:make-thread
                        (lambda () (message-processing-loop self))
                        :name  (mkstr "message-thread-" name)))))

(defun message-processing-loop (msgbox)
  "The message processing loop."
  (loop
     (pop-queue-and-process msgbox)
     (when (not (slot-value msgbox 'should-run)) (return))))

(defun pop-queue-and-process (msgbox)
  "This blocks until a new queue item arrived."
  (log:debug "Trying to pop from queue...")
  (with-slots (queue) msgbox
    (let ((item (queue:popq queue)))
      (log:debug "Got item: " item)
      (process-queue-item item)
      (incf (slot-value msgbox 'processed-messages)))))

(defun process-queue-item (item)
  (with-slots (message handler-fun withreply-p withreply-lock withreply-cvar) item
    (when handler-fun
      (if withreply-p
          (bt:with-lock-held (withreply-lock)
            ;; protect this to make sure the lock is released.
            (unwind-protect
                 (funcall handler-fun message)
              (bt:condition-notify withreply-cvar)))
          (funcall handler-fun message)))))

(defmethod submit ((self message-box-bt) message withreply-p handler-fun)
"Alternatively use `with-submit-handler' from your code to handle the message after it was 'popped' from the queue.
The `handler-fun' argument here will be `funcall'ed when the message was 'popped'."
  (log:debug "Submit message: " message)
  (with-slots (queue) self
    (if withreply-p
        (submit/reply queue message handler-fun)
        (submit/no-reply queue message handler-fun))))

(defun submit/no-reply (queue message handler-fun)
  (let ((push-item (make-message-item
                    :message message
                    :withreply-p nil
                    :withreply-lock nil
                    :withreply-cvar nil
                    :handler-fun handler-fun)))
    (log:debug "pushing item to queue:" push-item)
    (queue:pushq queue push-item)
    t))
  
(defun submit/reply (queue message handler-fun)
  (let* ((my-handler-result 'no-result)
         (my-handler-fun (lambda (msg)
                           ;; wrap the `handler-fun' so that we can get a function result.
                           (log:debug "Withreply: handler-fun...")
                           (setf my-handler-result (funcall handler-fun msg))
                           (log:debug "Withreply: handler-fun result: " my-handler-result)))
         (withreply-lock (bt:make-lock))
         (withreply-cvar (bt:make-condition-variable))
         (push-item (make-message-item
                     :message message
                     :withreply-p t
                     :withreply-lock withreply-lock
                     :withreply-cvar withreply-cvar
                     :handler-fun my-handler-fun)))

    (log:debug "Withreply: waiting for arrival of result...")
    (bt:with-lock-held (withreply-lock)
      (log:debug "pushing item to queue:" push-item)
      (queue:pushq queue push-item)
      (bt:condition-wait withreply-cvar withreply-lock)
      (log:debug "Withreply: result should be available: " my-handler-result))
    my-handler-result))

(defun wait-condition (cond-fun &optional (sleep-time 0.02) (max-time 12))
  (let ((wait-acc 0))
    (if (not (funcall cond-fun))
        (loop
           (sleep sleep-time)
           (+ sleep-time wait-acc)
           (when (or (funcall cond-fun) (> wait-acc max-time)) (return))))))

(defmethod stop ((self message-box-bt))
  (call-next-method)
  (with-slots (queue-thread should-run) self
    (setf should-run nil)
    (submit self :stop nil (lambda (msg) (declare (ignore msg))))))


;; ----------------------------------------
;; ------------- dispatcher msgbox---------
;; ----------------------------------------

(defclass message-box-dp (message-box-base)
  ((queue :initform nil
          :documentation
          "Which type of queue will be used depends on the `max-queue-size' setting.")
   (dispatcher :initarg :dispatcher
               :initform (error "Must be set!")
               :reader dispatcher
               :documentation "The dispatcher from the system.")
   (lock :initform (bt:make-lock)))
  (:documentation
"This message box is a message-box that uses the `system's `dispatcher'.
This has the advantage that an almost unlimited gservers/actors/agents can be created."))

(defmethod initialize-instance :after ((self message-box-dp) &key)
  (log:debug "Initialize instance: ~a~%" self)

  (with-slots (name queue queue-thread max-queue-size) self
    (log:debug "Requested max-queue-size: " max-queue-size)
    (setf queue
          (case max-queue-size
            ((0 nil) (make-instance 'queue-unbounded))
            (t (make-instance 'queue-bounded :max-items max-queue-size))))
    (log:info "Using queue: " queue)))


(defmethod submit ((self message-box-dp) message withreply-p handler-fun)
  (with-slots (name
               queue
               processed-messages
               dispatcher
               lock) self

    (log:debug "Enqueuing message: " message)
    (pushq queue message)

    (log:debug "Making dispatcher poping the queue and processing the message...")
    (incf processed-messages)

    (let ((dispatcher-fun (lambda ()
                            (funcall handler-fun (popq queue)))))
      (if withreply-p
          (dispatch dispatcher dispatcher-fun)
          (dispatch-async dispatcher dispatcher-fun)))))

(defmethod stop ((self message-box-dp))
  (call-next-method))
