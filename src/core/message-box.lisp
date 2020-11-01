(defpackage :cl-gserver.messageb
  (:use :cl :cl-gserver.utils :cl-gserver.queue)
  (:import-from #:disp
                #:dispatch
                #:dispatch-async)
  (:nicknames :mesgb)
  (:export #:message-box/dp #:message-box/bt
           #:submit
           #:with-submit-handler
           #:stop))

(in-package :cl-gserver.messageb)

(defclass message-box-base ()
  ((name :initform (string (gensym "mesgb-")))
   (processed-messages :initform 0)
   (queue :initform nil
          :documentation
          "Which type of queue will be used depends on the `max-queue-size' setting.")
   (max-queue-size :initform 0
                   :initarg :max-queue-size
                   :documentation
                   "0 or nil will make an unbounded queue. 
A value > 0 will make a bounded queue. 
Don't make it too small. A queue size of 1000 might be a good choice.")))

(defmethod initialize-instance :after ((self message-box-base) &key)
  (with-slots (queue max-queue-size) self
    (setf queue
          (case max-queue-size
            ((0 nil) (make-instance 'queue-unbounded))
            (t (make-instance 'queue-bounded :max-items max-queue-size)))))
  (log:debug "Initialize instance: ~a" self))

(defgeneric submit (message-box-base message withreply-p timeout handler-fun)
  (:documentation "Submit a message to the mailbox to be queued and handled."))

(defgeneric stop (message-box-base)
  (:documentation "Stops the message processing."))

(defmethod stop ((self message-box-base))
  (with-slots (processed-messages) self
    (log:debug "Processed messages: " processed-messages)))

(defmethod print-object ((obj message-box-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name processed-messages max-queue-size queue) obj
      (format stream "~a, processed messages: ~a, max-queue-size: ~a, queue: ~a"
              name
              processed-messages
              max-queue-size
              queue))))

;; ----------------------------------------
;; ------------- Generic ------------------
;; ----------------------------------------

(defmacro with-submit-handler ((msgbox message withreply-p timeout) &rest body)
  "Macro to let the caller specify a message handler function.
Use this instead of `submit'."
  `(submit ,msgbox ,message ,withreply-p ,timeout (lambda (message) ,@body)))

;; ----------------------------------------
;; ------------- Bordeaux ----------------
;; ----------------------------------------

(defstruct message-item
  (message nil)
  (withreply-p nil :type boolean)
  (withreply-lock nil)
  (withreply-cvar nil)
  (timeout nil)
  (handler-fun nil :type function))

(defclass message-box/bt (message-box-base)
  ((queue-thread :initform nil
                 :documentation
                 "The thread that pops queue items.")
   (should-run :initform t
               :documentation
               "Flag that indicates whether the message processing should commence."))
  (:documentation
   "Bordeaux-Threads based message-box with a single thread operating on a message queue.
This is used when the gserver is created outside of the `system'.
There is a limit on the maximum number of gservers/actors/agents that can be created with
this kind of queue because each message-box requires exactly one thread."))

(defmethod initialize-instance :after ((self message-box/bt) &key)
  (with-slots (name queue-thread) self
    (setf queue-thread (bt:make-thread
                        (lambda () (message-processing-loop self))
                        :name  (mkstr "message-thread-" name))))
  (when (next-method-p)
    (call-next-method)))

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
  (with-slots (message handler-fun withreply-p withreply-lock withreply-cvar timeout) item
    (when handler-fun
      (if withreply-p
          (bt:with-lock-held (withreply-lock)
            ;; protect this to make sure the lock is released.
            (unwind-protect
                 (if timeout
                     (bt:with-timeout (timeout) (funcall handler-fun message))
                     (funcall handler-fun message))
              (bt:condition-notify withreply-cvar)))
          (funcall handler-fun message)))))

(defmethod submit ((self message-box/bt) message withreply-p timeout handler-fun)
"Alternatively use `with-submit-handler' from your code to handle the message after it was 'popped' from the queue.
The `handler-fun' argument here will be `funcall'ed when the message was 'popped'."
  (log:trace "Submit message: " message)
  (with-slots (queue) self
    (if withreply-p
        (submit/reply queue message timeout handler-fun)
        (submit/no-reply queue message handler-fun))))

(defun submit/no-reply (queue message handler-fun)
  "This is quite efficient, no locking necessary."
  (let ((push-item (make-message-item
                    :message message
                    :withreply-p nil
                    :withreply-lock nil
                    :withreply-cvar nil
                    :timeout nil
                    :handler-fun handler-fun)))
    (log:debug "pushing item to queue:" push-item)
    (queue:pushq queue push-item)
    t))
  
(defun submit/reply (queue message timeout handler-fun)
  "This requires some more action. This function has to provide a result and so it's has to wait until
The queue thread has processed the message."
  (let* ((my-handler-result 'no-result)
         (my-handler-fun (lambda (msg)
                           ;; wrap the `handler-fun' so that we can get a function result.
                           (log:trace "Withreply: handler-fun...")
                           (setf my-handler-result (funcall handler-fun msg))
                           (log:trace "Withreply: handler-fun result: " my-handler-result)))
         (withreply-lock (bt:make-lock))
         (withreply-cvar (bt:make-condition-variable))
         (push-item (make-message-item
                     :message message
                     :withreply-p t
                     :withreply-lock withreply-lock
                     :withreply-cvar withreply-cvar
                     :timeout timeout
                     :handler-fun my-handler-fun)))

    (log:trace "Withreply: waiting for arrival of result...")
    (bt:with-lock-held (withreply-lock)
      (log:trace "pushing item to queue:" push-item)
      (queue:pushq queue push-item)
      (bt:condition-wait withreply-cvar withreply-lock)
      (log:trace "Withreply: result should be available: " my-handler-result))
    my-handler-result))

(defun wait-condition (cond-fun &optional (sleep-time 0.02) (max-time 12))
  (let ((wait-acc 0))
    (if (not (funcall cond-fun))
        (loop
           (sleep sleep-time)
           (+ sleep-time wait-acc)
           (when (or (funcall cond-fun) (> wait-acc max-time)) (return))))))

(defmethod stop ((self message-box/bt))
  (call-next-method)
  (with-slots (queue-thread should-run) self
    (setf should-run nil)
    (submit self :trigger-closing-the-wait-handler nil nil (lambda (msg) (declare (ignore msg))))))


;; ----------------------------------------
;; ------------- dispatcher msgbox---------
;; ----------------------------------------

(defclass message-box/dp (message-box-base)
  ((dispatcher :initarg :dispatcher
               :initform (error "Must be set!")
               :reader dispatcher
               :documentation "The dispatcher from the system.")
   (lock :initform (bt:make-lock)))
  (:documentation
   "This message box is a message-box that uses the `system's `dispatcher'.
This has the advantage that an almost unlimited gservers/actors/agents can be created.
This message-box doesn't 'own' a separate thread. It uses the `dispatcher' to handle the message processing.
The `dispatcher is kind of like a thread pool."))

(defmethod initialize-instance :after ((self message-box/dp) &key)
  (when (next-method-p)
    (call-next-method)))

(defmethod submit ((self message-box/dp) message withreply-p timeout handler-fun)
  "Submitting a message on a multi-threaded `dispatcher' is different as submitting on a single threaded message-box.
On a single threaded message-box the order of message processing is guaranteed even when submitting from multiple threads.
On the `dispatcher' this is not the case. The order cannot be guaranteed when messages are processed by different 
`dispatcher' threads. However, the we still guarantee a 'single-threadedness' regarding the state of the actor.
This is archieved here by protecting the `handler-fun' executation by a lock."
  (with-slots (name
               queue
               processed-messages
               dispatcher
               lock) self
    (incf processed-messages)
    (log:debug "Enqueuing message: " message)
    (pushq queue message)

    (let ((dispatcher-fun (lambda ()
                            (log:trace "Popping message...")
                            (let ((popped-msg (popq queue)))
                              (log:trace "Popping message...done")
                              (bt:acquire-lock lock t)
                              (unwind-protect
                                   (funcall handler-fun popped-msg)
                                (bt:release-lock lock))))))
      (if withreply-p
          (dispatch dispatcher dispatcher-fun)
          (dispatch-async dispatcher dispatcher-fun)))))

(defmethod stop ((self message-box/dp))
  (when (next-method-p)
    (call-next-method)))
