(defpackage :sento.messageb
  (:use :cl :sento.queue)
  (:import-from #:sento.miscutils
                #:mkstr)
  (:import-from #:timeutils
                #:ask-timeout)
  (:import-from #:disp
                #:dispatch-async
                #:throughput)
  (:nicknames :mesgb)
  (:export #:message-box/dp
           #:message-box/bt
           #:cancelled-p
           #:inner-msg
           #:submit
           #:stop
           #:*ask-s-spin-iterations*
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
With `withreply-p` the call blocks until the message was handled and returns the handler result.
Signals `ask-timeout` when `time-out` (seconds) elapses first, and `handler-unwound-error`
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
  (:documentation "Signaled by a synchronous `submit` when the message handler performed a
non-local exit (for example an `abort` restart was invoked, or the processing thread was
destroyed) so that no result exists."))

(defun call-handler-fun (handler-fun-args message)
  "`handler-fun-args` is a list with a function at `car` and args as `cdr`.
`message` is prepended to args.
This is used to break the environment possibly captured as closure at 'submit' stage."
  (when handler-fun-args
    (apply (car handler-fun-args) message (cdr handler-fun-args))))

(defun %submit-stop-trigger (msgbox)
  "Submits the stop-trigger message that wakes the processing loop so it
notices `should-run` has been cleared. With a bounded queue the queue may
already be full, in which case the trigger is unnecessary: the queued items
will be popped and drive the same re-check. A `queue-full-error` here is
therefore safely ignored so that stopping never fails because of a full queue."
  (handler-case
      (submit msgbox :trigger-ending-the-processing-loop nil nil nil)
    (queue-full-error ()
      (log:debug "~a: queue full on stop-trigger; relying on queued items to drain"
                 (name msgbox)))))

(defun %remaining-time (deadline)
  "Seconds left until DEADLINE, an internal real time."
  (/ (- deadline (get-internal-real-time))
     internal-time-units-per-second))

(defvar *ask-s-spin-iterations* 0
  "How often a synchronous `submit`, which is what `act:ask-s` does, re-reads
the state of its queued item before it parks the calling thread on a
condition-variable until the handler is done. 0, the default, parks right away.

Parking and waking a thread costs several microseconds. When the handler
usually finishes within that time, because it is short and the actor has no
backlog, a spin of some thousand reads returns with the result before the
thread is parked, which can double the `ask-s` throughput on a machine with
idle cores. When the reply takes longer than the spin, the spin is wasted CPU
on top of the park. Do not enable it on systems with more runnable threads
than cores, where the spinning caller takes CPU from the worker producing its
reply. Can be `let`-bound around a burst of `ask-s` calls.")

(defmacro %spin-until (form)
  "Evaluates FORM up to `*ask-s-spin-iterations*` times until it returns true."
  `(loop :repeat *ask-s-spin-iterations* :until ,form))

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
  ;; set by a timed `submit/reply` whose time-out elapsed; an item found
  ;; cancelled before its handler starts is not handled.
  (cancelled-p nil :type boolean)
  ;; set by the processing thread once the item is finished, whether or not
  ;; a result exists; `submit/reply` waits on this rather than on the bare
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
  "Starts the processing thread of MSGBOX. When that thread is unwound by a
non-local exit out of a handler, for example an `abort` restart, while the
message-box should still run and items are queued, it starts its successor
itself. Otherwise the queued items, and a run of a dispatcher message-box
scheduled on this worker, would wait for an unrelated submit to restart the
thread through `ensure-thread-is-running`."
  (with-slots (name queue queue-thread thread-is-running-p should-run)
      msgbox
    (flet ((run-processing-loop ()
             (setf thread-is-running-p t)
             (unwind-protect
                  (message-processing-loop msgbox)
               (setf thread-is-running-p nil)
               (when (and should-run (not (emptyq-p queue)))
                 (log:warn "~a: processing thread unwound with items queued, starting a new one"
                           name)
                 (start-thread msgbox
                               :thread-name (bt2:thread-name (bt2:current-thread)))))))
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
    (let ((item (popq queue)))
      (declare (type message-item/bt item))
      (when item
        (log:trace "~a: got item: ~a" (name msgbox) item)
        (process-queue-item msgbox item)
        (incf (slot-value msgbox 'processed-messages))))))

(defun %finalize-item/bt (item)
  "Marks ITEM as terminal and wakes the `submit/reply` caller waiting on it.
`done-p` is set under the item's lock so that the caller cannot miss it. The
notify happens after the lock is released so that the woken caller does not
immediately block on the lock this thread still holds."
  (declare (type message-item/bt item))
  (bt2:with-lock-held ((message-item/bt-withreply-lock item))
    (setf (message-item/bt-done-p item) t))
  (bt2:condition-notify (message-item/bt-withreply-cvar item)))

(defun process-queue-item (msgbox item)
  "Applies the `handler-fun-args` of ITEM to its message and stores the result
as its `handler-result`. The return of this function is not relevant.
An item whose timed `submit/reply` caller already gave up, `cancelled-p`, is
not handled. The handler runs without the item's lock held, so a caller whose
time-out elapses meanwhile is not delayed until the handler finishes; a
cancellation that arrives while the handler is already running does not
interrupt it, the handler completes and its result is discarded."
  (declare
   (type message-box/bt msgbox)
   (type message-item/bt item))
  (flet ((handler-fun ()
           (log:trace "~a: handler-fun-args..." (name msgbox))
           (setf (message-item/bt-handler-result item)
                 (call-handler-fun (message-item/bt-handler-fun-args item)
                                   (message-item/bt-message item)))
           (log:trace "~a: handler-fun-args result: ~a"
                      (name msgbox) (message-item/bt-handler-result item))))
    (if (message-item/bt-withreply-p item)
        ;; `done-p` is set and the waiter notified even when the handler
        ;; unwinds, so that `submit/reply` never waits forever.
        (unwind-protect
             (if (message-item/bt-cancelled-p item)
                 (log:warn "~a: item got cancelled, not handling message: ~a"
                           (name msgbox) (message-item/bt-message item))
                 (handler-fun))
          (%finalize-item/bt item))
        (handler-fun))))


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
  "This function has to provide a result and so it has to wait until the queue thread has processed the message. Processing of the queue item is done in `process-queue-item`.
`bt2:condition-wait` may return without a notification (spurious wakeup), so the wait is
repeated until the item's `done-p` is set. With a `time-out` the wait is bounded by an
absolute deadline so that spurious wakeups neither leak an unfinished result nor extend the timeout.
Signals `ask-timeout` when a timed wait expires, even if the handler completed while the lock
was being re-acquired, and `handler-unwound-error` when the item was processed but the
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
                     :handler-fun-args handler-fun-args
                     :handler-result 'no-result))
         (deadline (when time-out
                     (+ (get-internal-real-time)
                        (* time-out internal-time-units-per-second)))))
    (declare (type message-item/bt push-item))
    (log:trace "~a: pushing item to queue: ~a" (name msgbox) push-item)
    (pushq queue push-item)
    (ensure-thread-is-running msgbox)

    (log:trace "~a: withreply: waiting for arrival of result..." (name msgbox))
    (%spin-until (message-item/bt-done-p push-item))
    (bt2:with-lock-held (withreply-lock)
      (loop :until (message-item/bt-done-p push-item)
            :do (if deadline
                    (let ((remaining (%remaining-time deadline)))
                      (when (or (<= remaining 0)
                                (not (bt2:condition-wait withreply-cvar withreply-lock
                                                         :timeout remaining)))
                        (log:warn "~a: time-out elapsed but result not available yet!"
                                  (name msgbox))
                        (setf (message-item/bt-cancelled-p push-item) t)
                        (error 'ask-timeout :wait-time time-out)))
                    (bt2:condition-wait withreply-cvar withreply-lock))))

    (let ((handler-result (message-item/bt-handler-result push-item)))
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
    (pushq queue push-item)
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
  ;; only present for a submit with reply, so that `tell` does not pay for a
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
   (throughput :initarg :throughput
               :initform nil
               :reader throughput
               :documentation
               "The number of queued items one run on a dispatcher worker handles
before it yields the worker. Defaults to the dispatcher's `disp:throughput`.")
   (scheduled-p :initform (atomic:make-atomic-integer)
                :documentation
                "1 while a run of this message-box is scheduled on, or active on,
a dispatcher worker, 0 otherwise. A submit dispatches a run only when it
flips this from 0 to 1, so a burst of submits costs one dispatch.")
   (exec-fun-args :initform nil
                  :documentation
                  "The `dispatcher-exec-fun` call handed to `disp:dispatch-async`,
built once so that scheduling does not allocate.")
   (lock :initform (bt2:make-lock)
         :documentation
         "Held for the whole run of a batch. The scheduling flag already
keeps runs of one message-box from overlapping; the lock is what makes the
actor state written by one run visible to the next run on another worker
thread, and it keeps a custom dispatcher that runs the function inline from
racing a scheduled run."))
  (:documentation
   "This message box is a message-box that uses the `system`s `dispatcher`.
This has the advantage that an almost unlimited actors/agents can be created.
This message-box doesn't 'own' a thread. It uses the `dispatcher` to handle the message processing.
The `dispatcher` is kind of like a thread pool.

Message processing is scheduled per message-box, not per message: the first
submit on an idle message-box dispatches one run to a worker, further submits
while that run is pending only enqueue. A run takes up to `throughput` queued
items out of the queue at once, handles them, and reschedules the message-box
only when items remain."))

(defmethod initialize-instance :after ((self message-box/dp) &key)
  (with-slots (dispatcher throughput exec-fun-args) self
    (unless throughput
      (setf throughput (throughput dispatcher)))
    (check-type throughput (integer 1))
    (setf exec-fun-args (list 'dispatcher-exec-fun self)))
  (when (next-method-p)
    (call-next-method)))

(defun %finalize-item (item)
  "Marks ITEM as terminal and wakes a synchronous caller waiting on it.
Called for every popped item, including one that is cancelled or arrives after
the message-box stopped, so that a caller without a time-out never waits forever.
Items submitted without a reply have no lock and simply get flagged. With a
lock, `done-p` is set under it so that the caller cannot miss it, and the
notify happens after the lock is released so that the woken caller does not
immediately block on the lock this thread still holds."
  (declare (type message-item/dp item))
  (let ((lock (message-item/dp-lock item)))
    (if lock
        (progn
          (bt2:with-lock-held (lock)
            (setf (message-item/dp-done-p item) t))
          (bt2:condition-notify (message-item/dp-cvar item)))
        (setf (message-item/dp-done-p item) t))))

(defun wait-for-msg-handler-result (msgbox item)
  "Blocks until ITEM reached a terminal state or its `time-out` elapses.
Waits on the item's own condition-variable, re-checking `done-p` on every wakeup
because a wakeup may be spurious. With a `time-out` the wait is bounded by an
absolute deadline so that spurious wakeups do not extend it. Without a `time-out`
there is nothing to bound the wait.
Signals `ask-timeout` when the deadline passes first, even if the handler completed
while the lock was being re-acquired, and `handler-unwound-error` when the handler
was invoked but produced no result."
  (declare (type message-item/dp item))
  (let* ((lock (message-item/dp-lock item))
         (cvar (message-item/dp-cvar item))
         (time-out (message-item/dp-time-out item))
         (deadline (when time-out
                     (+ (get-internal-real-time)
                        (* time-out internal-time-units-per-second)))))
    (%spin-until (message-item/dp-done-p item))
    (bt2:with-lock-held (lock)
      (loop :until (message-item/dp-done-p item)
            :do (if deadline
                    (let ((remaining (%remaining-time deadline)))
                      (when (or (<= remaining 0)
                                (not (bt2:condition-wait cvar lock :timeout remaining)))
                        (log:warn "~a: time-out elapsed but result not available yet!"
                                  (name msgbox))
                        (setf (message-item/dp-cancelled-p item) t)
                        (error 'ask-timeout :wait-time time-out)))
                    (bt2:condition-wait cvar lock)))))
  (when (and (message-item/dp-handled-p item)
             (eq (message-item/dp-handler-result item) 'no-result))
    (let ((message (message-item/dp-message item)))
      (log:warn "~a: handler unwound without a result for message: ~a"
                (name msgbox) message)
      (error 'handler-unwound-error :message message))))

(defvar *schedule-max-attempts* 3
  "How many consecutive scheduling attempts `%schedule` makes for one burst
while the dispatcher keeps rejecting the run and the queue stays non-empty.
Bounds the retry that keeps a rejected dispatch from stranding items other
submitters already pushed while trusting this run to process them, without
spinning forever against a dispatcher that keeps rejecting.")

(defun %schedule (msgbox)
  "Dispatches a run of MSGBOX to a worker unless a run is already scheduled
or active. Exactly one submitter wins the 0 to 1 flip of `scheduled-p`.
When the dispatcher did not accept the run, the flag is cleared again and,
as long as the queue is still non-empty, scheduling is retried immediately,
up to `*schedule-max-attempts*` times: otherwise items other submitters
already pushed while trusting this run to process them would be stranded
until an unrelated future submit happens to retrigger scheduling. Once
attempts are exhausted the message-box is left idle for such a future submit
to try again."
  (with-slots (name scheduled-p dispatcher exec-fun-args queue) msgbox
    (loop :for attempt :from 1 :to *schedule-max-attempts*
          :while (atomic:atomic-cas scheduled-p 0 1)
          :do (let ((dispatched nil))
                (unwind-protect
                     (setf dispatched (eq t (dispatch-async dispatcher exec-fun-args)))
                  (unless dispatched
                    (atomic:atomic-cas scheduled-p 1 0)))
                (when dispatched
                  (return))
                (log:debug "~a: run not accepted by dispatcher ~a on attempt ~a/~a"
                           name dispatcher attempt *schedule-max-attempts*)
                (when (emptyq-p queue)
                  (return))))))

(defun %reschedule-when-pending (msgbox)
  "Marks MSGBOX idle after a run and schedules it again when items were
queued meanwhile. The flag is cleared before the queue is checked, so a
submit racing with the end of the run either sees the flag cleared and
dispatches itself, or its item is seen here. `emptyq-p` takes the queue lock,
which orders it after such a submit's push."
  (with-slots (scheduled-p queue) msgbox
    (atomic:atomic-cas scheduled-p 1 0)
    (unless (emptyq-p queue)
      (%schedule msgbox))))

(defun %requeue-unhandled (msgbox batch from below)
  "Puts the items of BATCH from index FROM below index BELOW back into the
queue of MSGBOX. Used when a run unwound out of a handler with items of its
batch still unhandled, so that they are neither lost nor left with a waiting
caller. They go behind items submitted since the batch was taken. An item
that does not fit into a full bounded queue is finalized unhandled instead."
  (with-slots (name queue) msgbox
    (log:warn "~a: run unwound with ~a of ~a batched items unhandled, requeuing them"
              name (- below from) below)
    (loop :for i :from from :below below
          :do (let ((item (shiftf (svref batch i) nil)))
                (handler-case
                    (pushq queue item)
                  (queue-full-error ()
                    (log:warn "~a: queue full, discarding unhandled message: ~a"
                              name (message-item/dp-message item))
                    (%finalize-item item)))))))

(defun dispatcher-exec-fun (msgbox)
  "One run of MSGBOX, executed on a dispatcher worker.
Takes up to `throughput` queued items out of the queue under one acquisition
of the queue lock and handles them under the message-box lock, applying the
`handler-fun-args` of each item to its message. Once the message-box is
stopped, the items are only finalized so that waiting callers are woken.
Afterwards the message-box is marked idle and rescheduled when items remain,
also when the run unwinds, so that the message-box never stays wedged; items
of the batch not handled by then are put back into the queue."
  (with-slots (name lock queue should-run throughput) msgbox
    (let ((batch (make-array throughput :initial-element nil))
          (count 0)
          (next 0))
      (declare (dynamic-extent batch)
               (type fixnum count next))
      (unwind-protect
           (bt2:with-lock-held (lock)
             (setf count (try-popq-into queue batch))
             (loop :while (< next count)
                   :do (let ((item (shiftf (svref batch next) nil)))
                         (incf next)
                         (if should-run
                             (handle-popped-item item msgbox)
                             (progn
                               (log:debug "~a: message-box stopped, discarding message: ~a"
                                          name (message-item/dp-message item))
                               (%finalize-item item))))))
        (when (< next count)
          (%requeue-unhandled msgbox batch next count))
        (%reschedule-when-pending msgbox)))))

(defun handle-popped-item (item msgbox)
  "Handles the popped message. Means: applies the function in `handler-fun-args` on the message.
The item is finalized on every exit path, including a handler that unwinds, so
that a synchronous caller waiting on it is always woken."
  (declare
   (type message-item/dp item)
   (type message-box/dp msgbox))
  (with-slots (name should-run) msgbox
    (log:trace "~a: popped message: ~a" name (message-item/dp-message item))
    (unwind-protect
         (if (and should-run (not (message-item/dp-cancelled-p item)))
             (progn
               (setf (message-item/dp-handled-p item) t)
               (setf (message-item/dp-handler-result item)
                     (call-handler-fun (message-item/dp-handler-fun-args item)
                                       (message-item/dp-message item))))
             (log:warn "~a: item got cancelled or message-box stopped, not handling message: ~a"
                       name (message-item/dp-message item)))
      (%finalize-item item))))

(defmethod submit ((self message-box/dp) message withreply-p time-out handler-fun-args)
  "Submitting a message on a multi-threaded `dispatcher` is different as submitting on a single threaded message-box. On a single threaded message-box the order of message processing is guaranteed even when submitting from multiple threads. On the `dispatcher` this is not the case. The order cannot be guaranteed when messages are processed by different `dispatcher` threads. However, we still guarantee a 'single-threadedness' regarding the state of the actor. This is achieved here by protecting the `handler-fun-args` execution with a lock.

The `time-out` with the 'dispatcher mailbox' assumes that the message received the dispatcher queue
and the handler in a reasonable amount of time, so that the effective time-out applies on the actual
handling of the message on the dispatcher queue thread.

Returns the handler-result if `withreply-p` is eq to `T`, otherwise the return is just `T` and is usually ignored.

With `withreply-p` the caller waits on the item's own state after scheduling.
A synchronous `dispatch` cannot be used here: under concurrent `ask-s` submits
a run handles whatever items head the queue, which is not necessarily this
caller's item, so a run's return value could belong to a different caller."
  (with-slots (name
               queue
               processed-messages) self
    (incf processed-messages)

    (let ((push-item (make-message-item/dp
                      :message message
                      :handler-fun-args handler-fun-args
                      :time-out time-out
                      :lock (when withreply-p (bt2:make-lock))
                      :cvar (when withreply-p (bt2:make-condition-variable)))))
      (log:debug "~a: enqueuing... withreply-p: ~a, time-out: ~a, message: ~a"
                 name withreply-p time-out message)
      (pushq queue push-item)
      (%schedule self)

      (if withreply-p
          (progn
            (wait-for-msg-handler-result self push-item)
            ;; an item that was never handled, because the message-box stopped
            ;; or the item was cancelled, has no result; `nil` is returned
            ;; rather than the internal sentinel.
            (if (message-item/dp-handled-p push-item)
                (message-item/dp-handler-result push-item)
                nil))
          t))))

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
