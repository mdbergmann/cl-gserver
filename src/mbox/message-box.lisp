(defpackage :sento.messageb
  (:use :cl :sento.queue)
  (:import-from #:sento.miscutils
                #:mkstr
                #:assert-cond
                #:await-cond)
  (:import-from #:timeutils
                #:ask-timeout)
  (:import-from #:disp
                #:dispatch
                #:dispatch-async)
  (:nicknames :mesgb)
  (:export #:message-box/dp
           #:message-box/bt
           #:cancelled-p
           #:inner-msg
           #:submit
           #:stop))

(in-package :sento.messageb)

(defclass message-box-base ()
  ((name :initform (string (gensym "mesgb-"))
         :initarg :name
         :reader name
         :documentation "The name of the message-box.
The default name is concatenated of \"mesgb-\" and a `gensym` generated random number.")
   (processed-messages :initform 0)
   (queue :initform nil
          :documentation
          "Which type of queue will be used depends on the `max-queue-size` setting.")
   (should-run :initform t
               :documentation
               "Flag that indicates whether the message processing should commence.")
   (max-queue-size :initform 0
                   :initarg :max-queue-size
                   :reader max-queue-size
                   :documentation
                   "0 or nil will make an unbounded queue.
A value `> 0` will make a bounded queue.
Don't make it too small. A queue size of 1000 might be a good choice."))
  (:documentation "The user does not need to create a message-box manually. It is automatically created and added to the `actor` when the actor is created through `ac:actor-of`."))

(defmethod initialize-instance :after ((self message-box-base) &key)
  (with-slots (queue max-queue-size) self
    (setf queue
          (case max-queue-size
            ((0 nil) (make-instance 'queue-unbounded))
            (t (make-instance 'queue-bounded :max-items max-queue-size))))))

(defmethod print-object ((obj message-box-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name processed-messages max-queue-size queue) obj
      (format stream "~a, processed messages: ~a, max-queue-size: ~a, queue: ~a"
              name
              processed-messages
              max-queue-size
              queue))))

(defgeneric submit (message-box-base message withreply-p time-out handler-fun-args)
  (:documentation "Submit a message to the mailbox to be queued and handled.
`handler-fun-args`: list with first element the function designator and rest arguments."))

(defgeneric stop (message-box-base &optional wait)
  (:documentation "Stops the message processing.
The message processing is not terminated while a message is still processed.
Rather it is a graceful stop by waiting until a message has been processed.
Provide `wait` EQ `T` to wait until the actor cell is stopped."))

(defmethod stop ((self message-box-base) &optional (wait nil))
  (declare (ignore wait))
  (with-slots (processed-messages) self
    (log:debug "~a: processed messages: ~a" (name self) processed-messages)))


;; ----------------------------------------
;; ------------- Generic ------------------
;; ----------------------------------------

(defun wait-and-probe-for-msg-handler-result (msgbox push-item)
  (with-slots (time-out handler-result cancelled-p) push-item
    (unless
        (assert-cond (lambda ()
                       (not (eq 'no-result handler-result)))
                     time-out 0.05)
      (log:warn "~a: time-out elapsed but result not available yet!" (name msgbox))
      (setf cancelled-p t)
      (error 'ask-timeout :wait-time time-out))))

(defun call-handler-fun (handler-fun-args message)
  "`handler-fun-args' is a list with a function at `car' and args as `cdr'.
`message' is prepended to args.
This is used to break the environment possibly captured as closure at 'submit' stage."
  (when handler-fun-args
    (let ((fun (car handler-fun-args))
          (args (cdr handler-fun-args)))
      (apply fun (cons message args)))))

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
  (handler-fun-args nil :type list)
  (handler-result 'no-result))

(defclass message-box/bt (message-box-base)
  ((queue-thread :initform nil
                 :documentation
                 "The thread that pops queue items.")
   (thread-lock :initform (bt2:make-lock)
                :documentation
                "A lock which should be taken when queue-thread slot is set."))
  (:documentation
   "Bordeaux-Threads based message-box with a single thread operating on a message queue.
This is used when the actor is created using a `:pinned` dispatcher type.
There is a limit on the maximum number of actors/agents that can be created with
this kind of queue because each message-box (and with that each actor) requires exactly one thread."))


(declaim (ftype (function (message-box/bt &key (:thread-name (or null string)))
                          (values &optional))
                start-thread))

(defun start-thread (msgbox &key thread-name)
  (with-slots (name queue-thread)
      msgbox
    (flet ((run-processing-loop ()
             (message-processing-loop msgbox)))
      (setf queue-thread
            (bt2:make-thread
             #'run-processing-loop
             :name (or thread-name
                       (mkstr "message-thread-" name))))))
  (values))


(defmethod initialize-instance :after ((self message-box/bt) &key)
  (start-thread self)
  
  (when (next-method-p)
    (call-next-method)))

(defun message-processing-loop (msgbox)
  "The message processing loop."
  (loop
    :while (slot-value msgbox 'should-run)
    :do (pop-queue-and-process msgbox)))

(defun pop-queue-and-process (msgbox)
  "This blocks until a new queue item arrived."
  (log:trace "~a: trying to pop from queue..." (name msgbox))
  (with-slots (queue) msgbox
    (let ((item (queue:popq queue)))
      (when item
        (log:trace "~a: got item: ~a" (name msgbox) item)
        (process-queue-item msgbox item)
        (incf (slot-value msgbox 'processed-messages))))))

(defun process-queue-item (msgbox item)
  "The `time-out' handling in here is to make sure that handling of the
message is 'interrupted' when the message was 'cancelled'.
This should happen in conjunction with the outer time-out in `submit/reply'.
This function sets the result as `handler-result' in `item'. The return of this function is not relevant."
  (with-slots (message
               handler-fun-args
               handler-result
               withreply-p
               withreply-lock
               withreply-cvar
               cancelled-p
               time-out) item
    (when cancelled-p
      (log:warn "~a: item got cancelled: ~a" (name msgbox) item)
      (when withreply-p
        (bt2:condition-notify withreply-cvar))
      (return-from process-queue-item :cancelled))

    (flet ((handler-fun ()
             (log:trace "~a: withreply: handler-fun-args..." (name msgbox))
             (setf handler-result
                   (call-handler-fun handler-fun-args message))
             (log:trace "~a: withreply: handler-fun-args result: ~a"
                        (name msgbox) handler-result)))
      (if withreply-p
          (bt2:with-lock-held (withreply-lock)
            ;; make sure we release the lock also on error
            (unwind-protect
                 (if time-out
                     (unless cancelled-p (handler-fun))
                     (handler-fun))
              (bt2:condition-notify withreply-cvar)))
          (handler-fun)))))


(declaim (ftype (function (message-box/bt)
                          (values &optional))
                ensure-thread-is-running))

(defun ensure-thread-is-running (msgbox)
  (with-slots (queue-thread thread-lock)
      msgbox
    (bt2:with-lock-held (thread-lock)
      (unless (bt2:thread-alive-p queue-thread)
        (log:trace "Restarting thread ~A"
                   (bt2:thread-name queue-thread))
        (start-thread msgbox
                      :thread-name (bt2:thread-name queue-thread)))
      (values))))
 

(defmethod submit ((self message-box/bt) message withreply-p time-out handler-fun-args)
  "The `handler-fun-args` argument must contain a handler function as first list item.
It will be apply'ed with the rest of the args when the message was 'popped' from queue."
  (log:trace "~a: submit message: ~a" (name self) message)
  (with-slots (queue) self
    (if withreply-p
        (submit/reply self queue message time-out handler-fun-args)
        (submit/no-reply self queue message handler-fun-args))))

(defun submit/reply (msgbox queue message time-out handler-fun-args)
  "This function has to provide a result and so it has to wait until the queue thread has processed the message. Processing of the queue item is done in `process-queue-item'."
  (let* ((withreply-lock (bt2:make-lock))
         (withreply-cvar (bt2:make-condition-variable))
         (push-item (make-message-item/bt
                     :message message
                     :withreply-p t
                     :withreply-lock withreply-lock
                     :withreply-cvar withreply-cvar
                     :time-out time-out
                     :handler-fun-args handler-fun-args
                     :handler-result 'no-result)))
    (cond
      (time-out
       (bt2:with-lock-held (withreply-lock)
         (log:trace "~a: pushing item to queue: ~a" (name msgbox) push-item)
         (queue:pushq queue push-item)
         ;; (ensure-thread-is-running msgbox)
         )

       ;; It is important to leave lock withreply-lock
       ;; before we will wait for result. Otherwisee handler-fun
       ;; will not be able to do it's job:
       (log:trace "~a: withreply: waiting for arrival of result..." (name msgbox))
       (wait-and-probe-for-msg-handler-result msgbox push-item))
      (t
       (bt2:with-lock-held (withreply-lock)
         (log:trace "~a: pushing item to queue: ~a" (name msgbox) push-item)
         (queue:pushq queue push-item)
         ;; (ensure-thread-is-running msgbox)

         (log:trace "~a: withreply: waiting for arrival of result..." (name msgbox))
         (bt2:condition-wait withreply-cvar withreply-lock))))

    (with-slots (handler-result) push-item
      (log:trace "~a: withreply: result should be available: ~a" (name msgbox) handler-result)
      handler-result)))

(defun submit/no-reply (msgbox queue message handler-fun-args)
  "This is quite efficient, no locking necessary.
If the message was submitted with timeout then the timeout plays no role here, the message is handled anyhow.
The submitting code has to await the side-effect and possibly handle a timeout."
  (let ((push-item (make-message-item/bt
                    :message message
                    :handler-fun-args handler-fun-args)))
    (log:trace "~a: pushing item to queue: ~a" (name msgbox) push-item)
    (queue:pushq queue push-item)
    t))

(defmethod stop ((self message-box/bt) &optional (wait nil))
  (when (next-method-p)
    (call-next-method))
  (with-slots (should-run queue-thread) self
    (setf should-run nil)
    (submit self :trigger-ending-the-processing-loop nil nil nil)
    (when wait
      (bt2:join-thread queue-thread))))

;; ----------------------------------------
;; ------------- dispatcher msgbox---------
;; ----------------------------------------

(defstruct message-item/dp
  (message nil)
  (time-out nil)
  (cancelled-p nil :type boolean)
  (handler-fun-args nil :type list)
  (handler-result 'no-result))

(defclass message-box/dp (message-box-base)
  ((dispatcher :initarg :dispatcher
               :initform (error "Dispatcher must be set!")
               :reader dispatcher
               :documentation
               "The dispatcher from the system.")
   (lock :initform (bt2:make-lock)))
  (:documentation
   "This message box is a message-box that uses the `system`s `dispatcher`.
This has the advantage that an almost unlimited actors/agents can be created.
This message-box doesn't 'own' a thread. It uses the `dispatcher` to handle the message processing.
The `dispatcher` is kind of like a thread pool."))

(defmethod initialize-instance :after ((self message-box/dp) &key)
  (when (next-method-p)
    (call-next-method)))

(defun dispatcher-exec-fun (msgbox)
  "This function is effectively executed on a dispatcher actor.
It knows the message-box of the origin actor and acts on it.
It pops the message from the message-boxes queue and applies the function in `handler-fun-args` on it.
The `handler-fun-args' is part of the message item."
  (with-slots (name lock queue should-run) msgbox
    (bt2:acquire-lock lock :wait t)
    (unwind-protect
         (progn
           (log:trace "~a: popping message..." name)
           (let ((popped-item (popq queue)))
             (when should-run
               (handle-popped-item popped-item msgbox))))
      (bt2:release-lock lock))))

(defun handle-popped-item (popped-item msgbox)
  "Handles the popped message. Means: applies the function in `handler-fun-args` on the message."
  (with-slots (name lock should-run) msgbox
    (with-slots (message cancelled-p handler-fun-args handler-result) popped-item
      (log:trace "~a: popped message: ~a" name popped-item)
      (unless (and should-run (not cancelled-p))
        (log:warn "~a: item got cancelled or message-box stopped: ~a" name popped-item)
        (return-from handle-popped-item))
      (when (and should-run (not cancelled-p))
        (setf handler-result (call-handler-fun handler-fun-args message))
        handler-result))))

(defmethod submit ((self message-box/dp) message withreply-p time-out handler-fun-args)
  "Submitting a message on a multi-threaded `dispatcher` is different as submitting on a single threaded message-box. On a single threaded message-box the order of message processing is guaranteed even when submitting from multiple threads. On the `dispatcher` this is not the case. The order cannot be guaranteed when messages are processed by different `dispatcher` threads. However, we still guarantee a 'single-threadedness' regarding the state of the actor. This is achieved here by protecting the `handler-fun-args` execution with a lock.

The `time-out` with the 'dispatcher mailbox' assumes that the message received the dispatcher queue
and the handler in a reasonable amount of time, so that the effective time-out applies on the actual
handling of the message on the dispatcher queue thread.

Returns the handler-result if `withreply-p' is eq to `T', otherwise the return is just `T' and is usually ignored."
  (with-slots (name
               queue
               processed-messages
               dispatcher) self
    (incf processed-messages)
    
    (let ((push-item (make-message-item/dp
                      :message message
                      :handler-fun-args handler-fun-args
                      :time-out time-out))
          (dispatcher-fun-args (list #'dispatcher-exec-fun self)))

      (log:debug "~a: enqueuing... withreply-p: ~a, time-out: ~a, message: ~a"
                 (name self) withreply-p time-out message)
      (pushq queue push-item)

      (if withreply-p
          (dispatch/reply self push-item dispatcher dispatcher-fun-args time-out)
          (dispatch/noreply self dispatcher dispatcher-fun-args)))))

(defun dispatch/reply (msgbox push-item dispatcher dispatcher-fun-args time-out)
  "Used by `ask-s'
Returns `handler-result'."
  (if time-out
      (dispatch/reply/timeout msgbox push-item dispatcher dispatcher-fun-args)
      (dispatch/reply/no-timeout msgbox dispatcher dispatcher-fun-args)))

(defun dispatch/reply/timeout (msgbox push-item dispatcher dispatcher-fun-args)
  "Waits for `handler-result' or timeout and returns `handler-result'."
  (dispatch-async dispatcher dispatcher-fun-args)
  (wait-and-probe-for-msg-handler-result msgbox push-item)
  (slot-value push-item 'handler-result))

(defun dispatch/reply/no-timeout (msgbox dispatcher dispatcher-fun-args)
  "Returns dispatcher result."
  (declare (ignore msgbox))
  (dispatch dispatcher dispatcher-fun-args))

(defun dispatch/noreply (msgbox dispatcher dispatcher-fun-args)
  "Used by `ask'.
Returns just `T'. Return is actually ignore."
  (declare (ignore msgbox))
  (dispatch-async dispatcher dispatcher-fun-args))

(defmethod stop ((self message-box/dp) &optional (wait nil))
  "Stop the message processing.
This discards further message processing on queued messages.
The message currently being processed will be processed to the end.
The `wait` flag has no consequence for the `dispatcher` message-box."
  (declare (ignore wait))
  (when (next-method-p)
    (call-next-method))
  (setf (slot-value self 'should-run) nil)
  (submit self :trigger-ending-the-processing-loop nil nil nil))
