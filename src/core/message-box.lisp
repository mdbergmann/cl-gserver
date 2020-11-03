(defpackage :cl-gserver.messageb
  (:use :cl :cl-gserver.utils :cl-gserver.queue)
  (:import-from #:disp
                #:dispatch
                #:dispatch-async)
  (:nicknames :mesgb)
  (:export #:message-box/dp
           #:message-box/bt
           #:delayed-cancellable-message
           #:make-delayed-cancellable-message
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

(defmethod print-object ((obj message-box-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name processed-messages max-queue-size queue) obj
      (format stream "~a, processed messages: ~a, max-queue-size: ~a, queue: ~a"
              name
              processed-messages
              max-queue-size
              queue))))

(defgeneric submit (message-box-base message withreply-p time-out handler-fun)
  (:documentation "Submit a message to the mailbox to be queued and handled."))

(defgeneric stop (message-box-base)
  (:documentation "Stops the message processing."))

(defmethod stop ((self message-box-base))
  (with-slots (processed-messages) self
    (log:debug "Processed messages: " processed-messages)))


;; ----------------------------------------
;; ------------- Generic ------------------
;; ----------------------------------------

(defmacro with-submit-handler ((msgbox message withreply-p time-out) &rest body)
  "Macro to let the caller specify a message handler function.
Use this instead of `submit'."
  `(submit ,msgbox ,message ,withreply-p ,time-out (lambda (message) ,@body)))


;; ----------------------------------------
;; Cancellable message
;; ----------------------------------------

(defclass delayed-cancellable-message ()
  ((inner-msg :initarg :inner-msg
              :initform nil
              :reader inner-msg)
   (cancelled-p :initarg :cancelled-p
                :initform nil
                :accessor cancelled-p
                :type boolean)
   (cancel-timer :initform nil
                 :accessor cancel-timer)
   (cancel-delay :initarg :cancel-delay
                 :initform nil
                 :reader cancel-delay
                 :documentation
                 "Delay after which the message gets cancelled and will not be processed.
If it has not been processed yet.")))

(defmethod initialize-instance :after ((self delayed-cancellable-message) &key)
  (setf (cancel-timer self)
        (make-timer (cancel-delay self)
                    (lambda () (setf (cancelled-p self) t)))))

(defun make-delayed-cancellable-message (inner-msg delay &optional cancelled-p)
  (make-instance 'delayed-cancellable-message
                 :inner-msg inner-msg
                 :cancel-delay delay
                 :cancelled-p cancelled-p))

;; ----------------------------------------
;; ------------- Bordeaux ----------------
;; ----------------------------------------

(defstruct message-item/bt
  (message nil)
  (withreply-p nil :type boolean)
  (withreply-lock nil)
  (withreply-cvar nil)
  (time-out nil)
  (cancelled-p nil :type boolean)
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
      (log:debug "Got item: ~a" item)
      (process-queue-item item)
      (incf (slot-value msgbox 'processed-messages)))))

(defun process-queue-item (item)
  "The `time-out' handling in here is to make sure that handling of the
message is 'interrupted'.
This should happen in conjunction with the outer time-out in `submit/reply'."
  (with-slots (message handler-fun withreply-p withreply-lock withreply-cvar cancelled-p time-out) item
    (when cancelled-p
      (log:warn "Item got cancelled: ~a" item)
      (when withreply-p
        (bt:condition-notify withreply-cvar))
      (return-from process-queue-item :cancelled))
    
    (when handler-fun
      (if withreply-p
          ;; protect this to make sure the lock is released.
          (bt:with-lock-held (withreply-lock)
            (unwind-protect
                 (if time-out
                     (unless cancelled-p (funcall handler-fun message))
                     (funcall handler-fun message))
              (bt:condition-notify withreply-cvar)))
          (funcall handler-fun message)))))

(defmethod submit ((self message-box/bt) message withreply-p time-out handler-fun)
"Alternatively use `with-submit-handler' from your code to handle the message after it was 'popped' from the queue.
The `handler-fun' argument here will be `funcall'ed when the message was 'popped'."
  (log:trace "Submit message: ~a" message)
  (with-slots (queue) self
    (if withreply-p
        (submit/reply queue message time-out handler-fun)
        (submit/no-reply queue message handler-fun))))

(defun submit/no-reply (queue message handler-fun)
  "This is quite efficient, no locking necessary."
  (let ((push-item (make-message-item/bt
                    :message message
                    :withreply-p nil
                    :withreply-lock nil
                    :withreply-cvar nil
                    :time-out nil
                    :cancelled-p nil
                    :handler-fun handler-fun)))
    (log:debug "pushing item to queue: ~a" push-item)
    (queue:pushq queue push-item)
    t))
  
(defun submit/reply (queue message time-out handler-fun)
  "This requires some more action. This function has to provide a result and so it's has to wait until
The queue thread has processed the message."
  (let* ((my-handler-result 'no-result)
         (my-handler-fun (lambda (msg)
                           ;; wrap the `handler-fun' so that we can get a function result.
                           (log:trace "Withreply: handler-fun...")
                           (setf my-handler-result (funcall handler-fun msg))
                           (log:trace "Withreply: handler-fun result: ~a" my-handler-result)))
         (withreply-lock (bt:make-lock))
         (withreply-cvar (bt:make-condition-variable))
         (push-item (make-message-item/bt
                     :message message
                     :withreply-p t
                     :withreply-lock withreply-lock
                     :withreply-cvar withreply-cvar
                     :time-out time-out
                     :cancelled-p nil
                     :handler-fun my-handler-fun)))

    (log:trace "Withreply: waiting for arrival of result...")
    (bt:with-lock-held (withreply-lock)
      (log:trace "pushing item to queue: ~a" push-item)
      (queue:pushq queue push-item)

      (when time-out
        (sleep time-out)  ;; submit/reply is a blocking call anyway, so we can sleep
        (when (eq 'no-result my-handler-result)
          (log:warn "Time-out elapsed but result not available yet!")
          (setf (slot-value push-item 'cancelled-p) t)
          (error 'utils:ask-timeout :wait-time time-out)))

      (bt:condition-wait withreply-cvar withreply-lock))
      (log:trace "Withreply: result should be available: ~a" my-handler-result)
    my-handler-result))

(defmethod stop ((self message-box/bt))
  (call-next-method)
  (with-slots (queue-thread should-run) self
    (setf should-run nil)
    (submit self :trigger-closing-the-wait-handler nil nil (lambda (msg) (declare (ignore msg))))))


;; ----------------------------------------
;; ------------- dispatcher msgbox---------
;; ----------------------------------------

(defstruct message-item/dp
  (message nil)
  (cancelled-p nil :type boolean)
  (handler-fun nil :type function))

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

(defun dispatcher-exec-fun (lock queue)
  "This function is effectively executed on a dispatcher actor."
  (log:trace "Popping message...")
  (let ((popped-item (popq queue)))
    (with-slots (message cancelled-p handler-fun) popped-item
      (log:debug "Popping message: ~a" popped-item)
      (when cancelled-p
        (log:warn "Item got cancelled: ~a" popped-item))
      (unless cancelled-p
        ;; protect the actor from concurrent state changes on the shared dispatcher
        (bt:acquire-lock lock t)
        (unwind-protect
             (unless cancelled-p (funcall handler-fun message))
          (bt:release-lock lock))))))

(defmethod submit ((self message-box/dp) message withreply-p time-out handler-fun)
  "Submitting a message on a multi-threaded `dispatcher' is different as submitting on a single threaded message-box.
On a single threaded message-box the order of message processing is guaranteed even when submitting from multiple threads.
On the `dispatcher' this is not the case. The order cannot be guaranteed when messages are processed by different 
`dispatcher' threads. However, we still guarantee a 'single-threadedness' regarding the state of the actor.
This is archieved here by protecting the `handler-fun' executation by a lock.

The `time-out' with the 'dispatcher mailbox' assumes that the message received the dispatcher queue
and the handler in a reasonable amount of time, so that the effective time-out applies on the actual
handling of the message on the dispatcher queue thread.

!!! attention: the `async-ask' uses no reply (here a `dispatch-async')."
  (with-slots (name
               queue
               processed-messages
               dispatcher
               lock) self
    (incf processed-messages)
    (let ((push-item (make-message-item/dp
                      :message message
                      :cancelled-p nil
                      :handler-fun handler-fun))
          (dispatcher-fun (lambda () (funcall #'dispatcher-exec-fun lock queue))))

      (log:info "Enqueuing... withreply-p: ~a, time-out: ~a, message: ~a" withreply-p time-out message)
      (pushq queue push-item)

      (if withreply-p
          (dispatch/reply push-item dispatcher dispatcher-fun time-out)
          (dispatch/noreply dispatcher dispatcher-fun)))))

(defun dispatch/reply (push-item dispatcher dispatcher-fun time-out)
  (if time-out
      (dispatch/reply/timeout time-out push-item dispatcher dispatcher-fun)
      (dispatch/reply/no-timeout dispatcher dispatcher-fun)))

(defun dispatch/reply/timeout (time-out push-item dispatcher dispatcher-fun)
  (handler-case
      (utils:with-waitfor (time-out)
        (dispatch dispatcher dispatcher-fun))
    (bt:timeout (c)
      (log:warn "Timeout: ~a" c)
      (setf (slot-value push-item 'cancelled-p) t)
      (error 'utils:ask-timeout :wait-time time-out :cause c))))

(defun dispatch/reply/no-timeout (dispatcher dispatcher-fun)
  (dispatch dispatcher dispatcher-fun))

(defun dispatch/noreply (dispatcher dispatcher-fun)
  "Used by `async-ask'."
  (dispatch-async dispatcher dispatcher-fun))

(defmethod stop ((self message-box/dp))
  (when (next-method-p)
    (call-next-method)))
