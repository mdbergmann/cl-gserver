(defpackage :sento.messageb
  (:use :cl :sento.queue)
  (:import-from #:sento.miscutils
                #:mkstr)
  (:import-from #:timeutils
                #:ask-timeout)
  (:import-from #:disp
                #:dispatch-async)
  (:nicknames :mesgb)
  (:export #:message-box/dp
           #:message-box/bt
           #:cancelled-p
           #:inner-msg
           #:submit
           #:stop
           ;; conditions
           #:handler-unwound-error
           #:message))

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
  (print-unreadable-object (obj stream :type stream)
    (with-slots (name processed-messages max-queue-size queue) obj
      (format stream "~a, processed messages: ~a, max-queue-size: ~a, queue: ~a"
              name
              processed-messages
              max-queue-size
              queue))))

(defgeneric submit (message-box-base message withreply-p time-out handler-fun-args)
  (:documentation "Submit a message to the mailbox to be queued and handled.
`handler-fun-args`: list with first element the function designator and rest arguments.
With `withreply-p' the call blocks until the message was handled and returns the handler result.
Signals `ask-timeout' when `time-out' (seconds) elapses first, and `handler-unwound-error'
when the handler exited without producing a result."))

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

(define-condition handler-unwound-error (error)
  ((message :initarg :message
            :reader message
            :documentation "The message whose handler exited without producing a result."))
  (:report (lambda (condition stream)
             (format stream "The handler for message '~a' unwound without producing a result!"
                     (message condition))))
  (:documentation "Signaled by a synchronous `submit' when the message handler performed a
non-local exit (for example an `abort' restart was invoked, or the processing thread was
destroyed) so that no result exists."))

(defun %check-handler-produced-result (msgbox push-item)
  "Signals `handler-unwound-error' when `push-item's handler was invoked but
produced no result, meaning it performed a non-local exit."
  (with-slots (message handled-p handler-result) push-item
    (when (and handled-p (eq handler-result 'no-result))
      (log:warn "~a: handler unwound without a result for message: ~a"
                (name msgbox) message)
      (error 'handler-unwound-error :message message))))

(defun %dispatched-handler-result (push-item)
  "The result to hand back to a synchronous caller of the dispatcher message-box.
An item that was never handled, because the message-box stopped or the item was
cancelled, has no result; `nil' is returned rather than the internal sentinel."
  (with-slots (handled-p handler-result) push-item
    (if handled-p handler-result nil)))

(defun wait-for-msg-handler-result (msgbox push-item)
  "Blocks until `push-item' reached a terminal state or its `time-out' elapses.
Waits on the item's own condition-variable, re-checking `done-p' on every wakeup
because a wakeup may be spurious. With a `time-out' the wait is bounded by an
absolute deadline so that spurious wakeups do not extend it. Without a `time-out'
there is nothing to bound the wait.
Signals `ask-timeout' when the deadline passes first, even if the handler completed
while the lock was being re-acquired, and `handler-unwound-error' when the handler
was invoked but produced no result."
  (with-slots (lock cvar done-p cancelled-p time-out) push-item
    (let ((deadline (when time-out
                      (+ (get-internal-real-time)
                         (* time-out internal-time-units-per-second)))))
      (bt2:with-lock-held (lock)
        (loop :until done-p
              :do (if deadline
                      (let ((remaining (/ (- deadline (get-internal-real-time))
                                          internal-time-units-per-second)))
                        (when (or (<= remaining 0)
                                  (not (bt2:condition-wait cvar lock :timeout remaining)))
                          (log:warn "~a: time-out elapsed but result not available yet!"
                                    (name msgbox))
                          (setf cancelled-p t)
                          (error 'ask-timeout :wait-time time-out)))
                      (bt2:condition-wait cvar lock))))))
  (%check-handler-produced-result msgbox push-item))

(defun %finalize-item (item)
  "Marks ITEM as terminal and wakes a synchronous caller waiting on it.
Called for every popped item, including one that is cancelled or arrives after
the message-box stopped, so that a caller without a time-out never waits forever.
Items submitted without a reply have no lock and simply get flagged."
  (with-slots (lock cvar done-p) item
    (if lock
        (bt2:with-lock-held (lock)
          (setf done-p t)
          (bt2:condition-notify cvar))
        (setf done-p t))))

(defun call-handler-fun (handler-fun-args message)
  "`handler-fun-args' is a list with a function at `car' and args as `cdr'.
`message' is prepended to args.
This is used to break the environment possibly captured as closure at 'submit' stage."
  (when handler-fun-args
    (let ((fun (car handler-fun-args))
          (args (cdr handler-fun-args)))
      (apply fun (cons message args)))))

(defun %submit-stop-trigger (msgbox)
  "Submits the stop-trigger message that wakes the processing loop so it
notices `should-run' has been cleared. With a bounded queue the queue may
already be full, in which case the trigger is unnecessary: the queued items
will be popped and drive the same re-check. A `queue-full-error' here is
therefore safely ignored so that stopping never fails because of a full queue."
  (handler-case
      (submit msgbox :trigger-ending-the-processing-loop nil nil nil)
    (queue-full-error ()
      (log:debug "~a: queue full on stop-trigger; relying on queued items to drain"
                 (name msgbox)))))

;; ----------------------------------------
;; ------------- Bordeaux ----------------
;; ----------------------------------------

(defvar *thread-destroy-waittime-s* 5.0
  "Default wait time until we destroy the thread.")

(defstruct message-item/bt
  (message nil)
  (withreply-p nil :type boolean)
  (withreply-lock nil :type (or null bt2:lock))
  (withreply-cvar nil :type (or null bt2:condition-variable))
  (time-out nil :type (or null number))
  (cancelled-p nil :type boolean)
  ;; set by the processing thread once the item is finished, whether or not
  ;; a result exists; `submit/reply' waits on this rather than on the bare
  ;; condition-variable wakeup, which may be spurious.
  (done-p nil :type boolean)
  (handler-fun-args nil :type list)
  (handler-result 'no-result))

(defclass message-box/bt (message-box-base)
  ((queue-thread :initform nil
                 :documentation
                 "The thread that pops queue items.")
   (thread-is-running-p :initform nil
                        :type boolean
                        :documentation
                        "Will be set to NIL if processing loop will be broken because of an error or a restart invocation."))
  (:documentation
   "Bordeaux-Threads based message-box with a single thread operating on a message queue.
This is used when the actor is created using a `:pinned` dispatcher type.
There is a limit on the maximum number of actors/agents that can be created with
this kind of queue because each message-box (and with that each actor) requires exactly one thread."))


(declaim (ftype (function (message-box/bt &key (:thread-name (or null string)))
                          (values &optional))
                start-thread))

(defun start-thread (msgbox &key thread-name)
  (with-slots (name queue-thread thread-is-running-p)
      msgbox
    (flet ((run-processing-loop ()
             (setf thread-is-running-p t)
             (unwind-protect
                  (message-processing-loop msgbox)
               (setf thread-is-running-p
                     nil))))
      (setf queue-thread
            (bt2:make-thread #'run-processing-loop
                             :name (or thread-name
                                       (mkstr "message-thread-" name))))))
  (values))


(defmethod initialize-instance :after ((self message-box/bt) &key)
  (start-thread self)
  
  (when (next-method-p)
    (call-next-method)))

(defun message-processing-loop (msgbox)
  "The message processing loop."
  (declare (type message-box/bt msgbox))
  (loop
    :while (slot-value msgbox 'should-run)
    :do (pop-queue-and-process msgbox)))

(defun pop-queue-and-process (msgbox)
  "This blocks until a new queue item arrived."
  (declare (type message-box/bt msgbox))
  (log:trace "~a: trying to pop from queue..." (name msgbox))
  (with-slots (queue) msgbox
    (let ((item (queue:popq queue)))
      (declare (type message-item/bt item))
      (when item
        (log:trace "~a: got item: ~a" (name msgbox) item)
        (process-queue-item msgbox item)
        (incf (slot-value msgbox 'processed-messages))))))

(defun process-queue-item (msgbox item)
  "The `time-out' handling in here is to make sure that handling of the
message is 'interrupted' when the message was 'cancelled'.
This should happen in conjunction with the outer time-out in `submit/reply'.
This function sets the result as `handler-result' in `item'. The return of this function is not relevant."
  (declare
   (type message-box/bt msgbox)
   (type message-item/bt item))
  (with-slots (message
               handler-fun-args
               handler-result
               withreply-p
               withreply-lock
               withreply-cvar
               cancelled-p
               done-p
               time-out) item
    (when cancelled-p
      (log:warn "~a: item got cancelled: ~a" (name msgbox) item)
      (when withreply-p
        (bt2:with-lock-held (withreply-lock)
          (setf done-p t)
          (bt2:condition-notify withreply-cvar)))
      (return-from process-queue-item :cancelled))

    (flet ((handler-fun ()
             (log:trace "~a: withreply: handler-fun-args..." (name msgbox))
             (setf handler-result
                   (call-handler-fun handler-fun-args message))
             (log:trace "~a: withreply: handler-fun-args result: ~a"
                        (name msgbox) handler-result)))
      (if withreply-p
          (bt2:with-lock-held (withreply-lock)
            ;; `done-p' is set and the waiter notified even when the handler
            ;; unwinds, so that `submit/reply' never waits forever.
            (unwind-protect
                 (if time-out
                     (unless cancelled-p (handler-fun))
                     (handler-fun))
              (setf done-p t)
              (bt2:condition-notify withreply-cvar)))
          (handler-fun)))))


(declaim (ftype (function (message-box/bt)
                          (values &optional))
                ensure-thread-is-running))

(defun ensure-thread-is-running (msgbox)
  (with-slots (queue-thread thread-is-running-p should-run)
      msgbox
    (when (and (not thread-is-running-p)
               should-run)
      ;; Just to be sure that thread is not alive:
      (unless (bt2:thread-alive-p queue-thread)
        (let ((thread-name (bt2:thread-name queue-thread)))
          (log:warn "Restarting thread" thread-name)
          (start-thread msgbox
                        :thread-name thread-name))))
    (values)))

(defmethod submit ((self message-box/bt) message withreply-p time-out handler-fun-args)
  "The `handler-fun-args` argument must contain a handler function as first list item.
It will be apply'ed with the rest of the args when the message was 'popped' from queue."
  (log:trace "~a: submit message: ~a" (name self) message)
  (with-slots (queue) self
    (if withreply-p
        (submit/reply self queue message time-out handler-fun-args)
        (submit/no-reply self queue message handler-fun-args))))

(defun submit/reply (msgbox queue message time-out handler-fun-args)
  "This function has to provide a result and so it has to wait until the queue thread has processed the message. Processing of the queue item is done in `process-queue-item'.
`bt2:condition-wait' may return without a notification (spurious wakeup), so the wait is
repeated until the item's `done-p' is set. With a `time-out' the wait is bounded by an
absolute deadline so that spurious wakeups neither leak an unfinished result nor extend the timeout.
Signals `ask-timeout' when a timed wait expires, even if the handler completed while the lock
was being re-acquired, and `handler-unwound-error' when the item was processed but the
handler produced no result."
  (declare
   (type message-box/bt msgbox)
   (type (or null number) time-out)
   (type list handler-fun-args))
  (let* ((withreply-lock (bt2:make-lock))
         (withreply-cvar (bt2:make-condition-variable))
         (push-item (make-message-item/bt
                     :message message
                     :withreply-p t
                     :withreply-lock withreply-lock
                     :withreply-cvar withreply-cvar
                     :time-out time-out
                     :handler-fun-args handler-fun-args
                     :handler-result 'no-result))
         (deadline (when time-out
                     (+ (get-internal-real-time)
                        (* time-out internal-time-units-per-second)))))
    (declare (type message-item/bt push-item))
    (with-slots (done-p cancelled-p handler-result) push-item
      (bt2:with-lock-held (withreply-lock)
        (log:trace "~a: pushing item to queue: ~a" (name msgbox) push-item)
        (queue:pushq queue push-item)
        (ensure-thread-is-running msgbox)

        (log:trace "~a: withreply: waiting for arrival of result..." (name msgbox))
        (loop :until done-p
              :do (let ((remaining (when deadline
                                     (/ (- deadline (get-internal-real-time))
                                        internal-time-units-per-second))))
                    (when (and remaining
                               (or (<= remaining 0)
                                   (not (bt2:condition-wait withreply-cvar withreply-lock
                                                            :timeout remaining))))
                      (log:warn "~a: time-out elapsed but result not available yet!"
                                (name msgbox))
                      (setf cancelled-p t)
                      (error 'ask-timeout
                             :wait-time time-out))
                    (unless remaining
                      (bt2:condition-wait withreply-cvar withreply-lock)))))

      (log:trace "~a: withreply: result available: ~a" (name msgbox) handler-result)
      (when (eq handler-result 'no-result)
        (log:warn "~a: handler unwound without a result for message: ~a"
                  (name msgbox) message)
        (error 'handler-unwound-error :message message))
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
    (ensure-thread-is-running msgbox)
    t))

(defmethod stop ((self message-box/bt) &optional (wait nil))
  (when (next-method-p)
    (call-next-method))
  (with-slots (should-run queue-thread name) self
    (setf should-run nil)
    (%submit-stop-trigger self)

    (let ((observer (bt2:make-thread
                     (lambda ()
                       (timeutils:wait-cond
                        (lambda ()
                          (not (bt2:thread-alive-p queue-thread)))
                        0.05 *thread-destroy-waittime-s*)
                       (when (bt2:thread-alive-p queue-thread)
                         (log:warn "Thread on mesgb '~a' won't stop after ~a seconds. Forcing stop!"
                                   name *thread-destroy-waittime-s*)
                         (bt2:destroy-thread queue-thread)))
                     :name "msgb-thread-stop-observer")))
      (when wait
        (bt2:join-thread observer)))))

;; ----------------------------------------
;; ------------- dispatcher msgbox---------
;; ----------------------------------------

(defstruct message-item/dp
  (message nil)
  (time-out nil :type (or null number))
  (cancelled-p nil :type boolean)
  ;; set once the item reached a terminal state, whether it was handled or not.
  (done-p nil :type boolean)
  ;; set when the handler was actually invoked, which separates 'the handler
  ;; produced no result' from 'the item was never handled'.
  (handled-p nil :type boolean)
  ;; only present for a submit with reply, so that `tell' does not pay for a
  ;; lock and a condition-variable per message.
  (lock nil :type (or null bt2:lock))
  (cvar nil :type (or null bt2:condition-variable))
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
             (when popped-item
               (if should-run
                   (handle-popped-item popped-item msgbox)
                   (progn
                     (log:warn "~a: message-box stopped, not handling: ~a"
                               name popped-item)
                     (%finalize-item popped-item))))))
      (bt2:release-lock lock))))

(defun handle-popped-item (popped-item msgbox)
  "Handles the popped message. Means: applies the function in `handler-fun-args` on the message.
The item is finalized on every exit path, including a handler that unwinds, so
that a synchronous caller waiting on it is always woken."
  (declare
   (type message-item/dp popped-item)
   (type message-box/dp msgbox))
  (with-slots (name should-run) msgbox
    (with-slots (message cancelled-p handled-p handler-fun-args handler-result) popped-item
      (log:trace "~a: popped message: ~a" name popped-item)
      (unwind-protect
           (progn
             (unless (and should-run (not cancelled-p))
               (log:warn "~a: item got cancelled or message-box stopped: ~a"
                         name popped-item)
               (return-from handle-popped-item))
             (setf handled-p t)
             (setf handler-result (call-handler-fun handler-fun-args message)))
        (%finalize-item popped-item)))))

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
                      :time-out time-out
                      :lock (when withreply-p (bt2:make-lock))
                      :cvar (when withreply-p (bt2:make-condition-variable))))
          (dispatcher-fun-args (list #'dispatcher-exec-fun self)))

      (log:debug "~a: enqueuing... withreply-p: ~a, time-out: ~a, message: ~a"
                 (name self) withreply-p time-out message)
      (pushq queue push-item)

      (if withreply-p
          (dispatch/reply self push-item dispatcher dispatcher-fun-args)
          (dispatch/noreply self dispatcher dispatcher-fun-args)))))

(defun dispatch/reply (msgbox push-item dispatcher dispatcher-fun-args)
  "Used by `ask-s'. Waits for `handler-result', or the item's `time-out', and returns it.
Dispatches asynchronously and then waits on `push-item's own state. A synchronous
`dispatch' cannot be used here: under concurrent `ask-s' submits on a `:shared'
dispatcher, `dispatch' pops whatever item is at the head of the queue, which is not
necessarily `push-item', so its return value could belong to a different caller's
message entirely."
  (dispatch-async dispatcher dispatcher-fun-args)
  (wait-for-msg-handler-result msgbox push-item)
  (%dispatched-handler-result push-item))

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
  (%submit-stop-trigger self))
