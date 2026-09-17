(defpackage :sento.message-box-test
  (:use :cl :fiveam :cl-mock :sento.actor :sento.future)
  (:shadow #:! #:?)
  (:import-from #:miscutils
                #:assert-cond
                #:await-cond
                #:filter)
  (:import-from #:timeutils
                #:ask-timeout)
  (:import-from #:sento.messageb
                #:message-box/bt
                #:submit
                #:handler-unwound-error
                #:queue-thread
                #:stop)
  (:import-from #:sento.queue
                #:queued-count)
  (:import-from #:sento.test-utils
                #:parametrized-test)
  (:import-from #:ac
                #:actor-of))

(in-package :sento.message-box-test)

(def-suite message-box-tests
  :description "message-box tests"
  :in sento.tests:test-suite)

(in-suite message-box-tests)


(defun wait-while-thread-will-die (msgbox &key (timeout 10))
  (let ((wait-until (+ (get-internal-real-time) (* timeout
                                                   internal-time-units-per-second))))
    (with-slots (queue-thread)
        msgbox
      (loop :while (bt2:thread-alive-p queue-thread)
            :do (sleep 0.1)
                (when (< wait-until
                         (get-internal-real-time))
                  (error "Thread didn't die in ~A seconds."
                         timeout))))))


(parametrized-test bt-box-resurrects-thread-after-abort-if-handler-catches-all-signals
    ((withreply-p timeout)
     (nil         nil)
     (t           1)
     (t           nil))
  
  "Simulates a situation when error has happened during message processing, and ABORT restart was invoked.
   Usually this kill a thread, but here we ensure that the thread is resurrected when we submit a
   subsequent message."

  (flet ((kill-by-restart-invoke (msg)
           (declare (ignore msg))
           (handler-case
               ;; This way we are simulating that the user choose
               ;; an ABORT restart in the IDE during debug session:
               (handler-bind ((serious-condition #'abort))
                 (error "Die, thread, die!"))
             ;; This part the same as error handling code in the
             ;; SENTO.ACTOR-CELL:HANDLE-MESSAGE function:
             ;; 
             ;; TODO: t was used to check if it is able to
             ;; catch stack unwinding because of INVOKE-RESTART,
             ;; but it can't.
             (t (c)
               (log:error "error condition was raised: ~%~a~%"
                          c)
               (cons :handler-error c)))))
    
    (let ((box (make-instance 'message-box/bt
                              :name "foo")))
      (unwind-protect
           (progn
             (let ((first-reply
                     (handler-case
                         (submit box "The Message"
                                 t
                                 ;; No timeout here: the handler unwinds the
                                 ;; thread and never returns a result, which
                                 ;; must be reported instead of waiting forever.
                                 nil
                                 (list #'kill-by-restart-invoke))
                       (handler-unwound-error (c)
                         (list :unwound (sento.messageb:message c))))))
               (is (equal '(:unwound "The Message")
                          first-reply)))

             (wait-while-thread-will-die box)

             (is (not
                  (bt2:thread-alive-p
                   (slot-value box 'queue-thread))))

             (let ((result (handler-case
                               (submit box "The Message"
                                       withreply-p
                                       timeout
                                       (list (lambda (msg)
                                               (reverse msg))))
                             (ask-timeout ()
                               :timeout))))

               (cond
                 (withreply-p
                  (is (string= "egasseM ehT" result)))
                 (t
                  (is (eql result t)))))

             (is (bt2:thread-alive-p
                  (slot-value box 'queue-thread))))

        ;; Cleanup a thread:
        (stop box t)))))


(def-fixture spurious-condition-wait (spurious-count)
  "Makes `bt2:condition-wait' return `T' without waiting for the first
SPURIOUS-COUNT calls made from the test thread. This simulates the spurious
wakeups that condition variables are allowed to produce. Calls from other
threads (the message-box thread waiting on the queue) reach the real function."
  (let ((test-thread (bt2:current-thread))
        (spurious-left spurious-count))
    (with-mocks (:recordp nil)
      (answer bt2:condition-wait
        (if (and (eq test-thread (bt2:current-thread))
                 (plusp spurious-left))
            (progn
              (decf spurious-left)
              t)
            (call-previous)))
      (&body))))

(test submit/reply--spurious-wakeup--no-timeout
  "Spurious `condition-wait' wakeups must not leak the internal 'no result'
sentinel: the reply is the handler result once it is actually available."
  (let ((box (make-instance 'message-box/bt :name "spurious")))
    (unwind-protect
         (with-fixture spurious-condition-wait (3)
           (let ((result (submit box "The Message" t nil (list #'reverse))))
             (is (string= "egasseM ehT" result))
             (is (= 0 spurious-left))))
      (stop box t))))

(test submit/reply--spurious-wakeup--with-timeout
  "Spurious wakeups before the timeout elapses are neither reported as a
result nor as a timeout."
  (let ((box (make-instance 'message-box/bt :name "spurious-timeout")))
    (unwind-protect
         (with-fixture spurious-condition-wait (3)
           (let ((result (handler-case
                             (submit box "The Message" t 1 (list #'reverse))
                           (ask-timeout () :timeout))))
             (is (string= "egasseM ehT" result))
             (is (= 0 spurious-left))))
      (stop box t))))

(test submit/reply--spurious-wakeup--timeout-still-fires
  "With the message-box thread busy, spurious wakeups must not leak the
sentinel and the timeout must still be signaled before the busy handler ends."
  (let ((box (make-instance 'message-box/bt :name "spurious-busy")))
    (unwind-protect
         (progn
           (submit box "busy" nil nil (list (lambda (msg)
                                             (declare (ignore msg))
                                             (sleep 1))))
           (with-fixture spurious-condition-wait (3)
             (let* ((start (get-internal-real-time))
                    (result (handler-case
                                (submit box "The Message" t 0.2 (list #'reverse))
                              (ask-timeout () :timeout)))
                    (elapsed (/ (- (get-internal-real-time) start)
                                internal-time-units-per-second)))
               (is (eq :timeout result))
               (is (= 0 spurious-left))
               (is (< elapsed 0.8) "timeout took ~a seconds" elapsed))))
      (stop box t))))

(test submit/reply--timeout--not-delayed-by-running-handler
  "The message-box/bt runs the handler without the item's lock held, so a
timed `submit' whose own handler is still running signals `ask-timeout' at
the deadline instead of after the handler finished."
  (let ((box (make-instance 'message-box/bt :name "slow-handler"))
        (handled nil))
    (unwind-protect
         (let* ((start (get-internal-real-time))
                (result (handler-case
                            (submit box "The Message" t 0.2
                                    (list (lambda (msg)
                                            (sleep 1)
                                            (setf handled msg))))
                          (ask-timeout () :timeout)))
                (elapsed (/ (- (get-internal-real-time) start)
                            internal-time-units-per-second)))
           (is (eq :timeout result))
           (is (< elapsed 0.8) "timeout took ~a seconds" elapsed)
           ;; the handler is not interrupted by the cancellation, it completes
           (is-true (await-cond 1.5 (string= "The Message" handled))))
      (stop box t))))

(test submit/reply--cancelled-before-handled--not-handled
  "An item whose timed `submit' gave up before the message-box thread got to
it is skipped: its handler never runs."
  (let ((box (make-instance 'message-box/bt :name "cancelled"))
        (handled nil))
    (unwind-protect
         (flet ((record (msg) (push msg handled)))
           (submit box "busy" nil nil (list (lambda (msg)
                                             (declare (ignore msg))
                                             (sleep 0.5))))
           (is (eq :timeout
                   (handler-case
                       (submit box "The Message" t 0.1 (list #'record))
                     (ask-timeout () :timeout))))
           (submit box "after" nil nil (list #'record))
           (is-true (await-cond 1.0 handled))
           (is (equal '("after") handled)))
      (stop box t))))

(test message-box/bt--thread-unwinds-with-queued-items--restarts-itself
  "When the processing thread is unwound by a handler, here through the
`abort' restart, while items are still queued, the message-box starts a new
processing thread right away so that the queued items are handled without
waiting for a further submit."
  (let ((box (make-instance 'message-box/bt :name "self-restart"))
        (release nil)
        (handled nil))
    (unwind-protect
         (progn
           (submit box :abort nil nil (list (lambda (msg)
                                             (declare (ignore msg))
                                             (loop :until release :do (sleep 0.01))
                                             (abort))))
           (submit box 1 nil nil (list (lambda (msg) (push msg handled))))
           (submit box 2 nil nil (list (lambda (msg) (push msg handled))))
           (setf release t)
           (is-true (await-cond 1.0 (= 2 (length handled))))
           (is (equal '(1 2) (reverse handled))))
      (stop box t))))

(parametrized-test ask-s--spin-before-park
    ((dispatcher)
     (:pinned)
     (:shared))
  "With `*ask-s-spin-iterations*' set, `ask-s' returns the result on both
message-box kinds, and a timed `ask-s' on a slow handler still times out at
its deadline."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 2)))))
        (mesgb:*ask-s-spin-iterations* 10000))
    (unwind-protect
         (let ((actor (actor-of system
                                :receive (lambda (msg)
                                           (when (eq msg :slow) (sleep 1))
                                           msg)
                                :dispatcher dispatcher)))
           (is (equal (loop :for i :from 0 :below 20 :collect i)
                      (loop :for i :from 0 :below 20 :collect (ask-s actor i))))
           (is (= 42 (ask-s actor 42 :time-out 1)))
           (let* ((start (get-internal-real-time))
                  (result (ask-s actor :slow :time-out 0.2))
                  (elapsed (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second)))
             (is (eq :handler-error (car result)))
             (is (typep (cdr result) 'ask-timeout))
             (is (< elapsed 0.8) "timeout took ~a seconds" elapsed)))
      (ac:shutdown system))))

(def-fixture dp-actor (receive)
  "An actor on a `:shared' dispatcher, so that `ask-s' goes through the
dispatcher message-box, with the system shut down afterwards."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 2))))))
    (unwind-protect
         (let ((actor (actor-of system :receive receive)))
           (&body))
      (ac:shutdown system))))

(test dispatch/reply--spurious-wakeup--with-timeout
  "The dispatcher message-box waits for a timed `ask-s' on a condition-variable.
Spurious wakeups before the timeout elapses are neither reported as a result nor
as a timeout."
  (with-fixture dp-actor (#'reverse)
    (with-fixture spurious-condition-wait (3)
      (let ((result (ask-s actor "The Message" :time-out 1)))
        (is (string= "egasseM ehT" result))
        (is (= 0 spurious-left))))))

(test dispatch/reply--spurious-wakeup--timeout-still-fires
  "With a slow handler, spurious wakeups must not extend a timed `ask-s' on the
dispatcher message-box beyond its deadline."
  (with-fixture dp-actor ((lambda (msg) (sleep 1) msg))
    (with-fixture spurious-condition-wait (3)
      (let* ((start (get-internal-real-time))
             (result (ask-s actor "The Message" :time-out 0.2))
             (elapsed (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)))
        (is (eq :handler-error (car result)))
        (is (typep (cdr result) 'ask-timeout))
        (is (= 0 spurious-left))
        (is (< elapsed 0.8) "timeout took ~a seconds" elapsed)))))

(def-fixture dispatch-counter (dispatcher)
  "Counts the calls to `disp:dispatch-async' on DISPATCHER made while the body
runs, from any thread, in `dispatches'. The calls are passed on to the real
dispatcher. The mock is global, so calls on other dispatchers are not counted:
an actor of an earlier test whose handler outlives its system's shutdown
reschedules itself against the stopped workers when the handler finally
returns, and those attempts would otherwise be attributed to this test."
  (let ((dispatches 0)
        (count-lock (bt2:make-lock)))
    (with-mocks (:recordp nil)
      (answer (disp:dispatch-async disp args)
        (progn
          (when (eq disp dispatcher)
            (bt2:with-lock-held (count-lock)
              (incf dispatches)))
          (call-previous disp args)))
      (&body))))

(test dispatch--batch--one-dispatch-per-burst
  "Submits on a message-box whose run is already scheduled only enqueue. The
run drains up to `throughput' items and reschedules the message-box only while
items remain: 21 messages with a throughput of 5 take 5 runs, not 21."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 2 :throughput 5))))))
    (unwind-protect
         (let* ((release nil)
                (processed 0)
                (actor (actor-of system
                                 :receive (lambda (msg)
                                            (when (eq msg :block)
                                              (loop :until release :do (sleep 0.01)))
                                            (incf processed)))))
           (with-fixture dispatch-counter ((getf (asys:dispatchers system) :shared))
             (tell actor :block)
             (is-true (await-cond 0.5 (= 1 dispatches)))
             (loop :repeat 20 :do (tell actor :go))
             (is (= 1 dispatches))
             (setf release t)
             (is-true (await-cond 1.0 (= 21 processed)))
             (is (= 5 dispatches))))
      (ac:shutdown system))))

(test dispatch--batch--preserves-order
  "Batched runs keep the order in which one sender submitted its messages."
  (let ((received nil))
    (with-fixture dp-actor ((lambda (msg) (push msg received)))
      (loop :for i :from 0 :below 100 :do (tell actor i))
      (is-true (await-cond 1.0 (= 100 (length received))))
      (is (equal (loop :for i :from 0 :below 100 :collect i)
                 (reverse received))))))

(test dispatch--batch--yields-worker-after-throughput
  "With one worker and a throughput of 2, a second actor's message is handled
between two batches of the first actor's backlog, not after the whole backlog.
A run takes its batch out of the queue at once, so the backlog has to be
complete before the first run of `a' starts: a gate actor holds the only
worker until all messages are queued."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 1 :throughput 2))))))
    (unwind-protect
         (let* ((release nil)
                (order nil)
                (order-lock (bt2:make-lock))
                (gate (actor-of system
                                :receive (lambda (msg)
                                           (declare (ignore msg))
                                           (loop :until release :do (sleep 0.01)))))
                (a (actor-of system
                             :receive (lambda (msg)
                                        (bt2:with-lock-held (order-lock)
                                          (push (cons :a msg) order)))))
                (b (actor-of system
                             :receive (lambda (msg)
                                        (bt2:with-lock-held (order-lock)
                                          (push (cons :b msg) order))))))
           (tell gate :block)
           (tell a 1)
           (tell b :x)
           (loop :for i :from 2 :to 4 :do (tell a i))
           (setf release t)
           (is-true (await-cond 1.0 (= 5 (length order))))
           (is (equal '((:a . 1) (:a . 2) (:b . :x) (:a . 3) (:a . 4))
                      (reverse order))))
      (ac:shutdown system))))

(test dispatch--failed-dispatch--does-not-wedge-mailbox
  "A dispatch the dispatcher did not accept clears the scheduled flag again,
so the next submit schedules the message-box and both items are handled."
  (with-fixture dp-actor (#'identity)
    (let ((fail-once t))
      (with-mocks (:recordp nil)
        (answer disp:dispatch-async
          (if fail-once
              (progn
                (setf fail-once nil)
                nil)
              (call-previous)))
        (tell actor 1)
        (is (= 2 (ask-s actor 2 :time-out 1)))))))

(test dispatch--failed-dispatch--retries-without-next-submit
  "When the dispatcher rejects a run while the queue stays non-empty,
`%schedule' retries immediately instead of depending on some future,
unrelated submit to retrigger scheduling. Here only one submit happens: with
two transient rejections followed by success, the item must still be
processed within the retry bound."
  (with-fixture dp-actor (#'identity)
    (let ((fail-count 2))
      (with-mocks (:recordp nil)
        (answer disp:dispatch-async
          (if (plusp fail-count)
              (progn (decf fail-count) nil)
              (call-previous)))
        (is (= 42 (ask-s actor 42 :time-out 1)))))))

(test dispatch--batch--handler-unwinds--requeues-remaining-items
  "A run takes its batch out of the queue at once. When a handler unwinds the
run, here by invoking the `abort' restart which also ends the worker thread,
the items of the batch not handled yet are put back into the queue and
handled by a later run instead of being lost."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 1 :throughput 10))))))
    (unwind-protect
         (let* ((block-release nil)
                (block-started nil)
                (abort-release nil)
                (abort-started nil)
                (handled nil)
                (actor (actor-of system
                                 :receive (lambda (msg)
                                            (cond
                                              ((eq msg :block)
                                               (setf block-started t)
                                               (loop :until block-release :do (sleep 0.01)))
                                              ((eq msg :abort)
                                               (setf abort-started t)
                                               (loop :until abort-release :do (sleep 0.01))
                                               (abort))
                                              (t (push msg handled)))
                                            msg)))
                (msgbox-queue (slot-value (act-cell:msgbox actor) 'mesgb::queue)))
           ;; Occupy the sole worker with a run whose own batch is just
           ;; `:block', so that `:abort' and 1/2/3, told while it is busy,
           ;; accumulate in the queue and are taken as one batch by the next
           ;; run's `try-popq-into' instead of being popped one at a time.
           (tell actor :block)
           (is-true (await-cond 0.5 block-started))
           (tell actor :abort)
           (loop :for i :from 1 :to 3 :do (tell actor i))
           (is-true (await-cond 0.5 (= 4 (queued-count msgbox-queue))))
           (setf block-release t)
           (is-true (await-cond 0.5 abort-started))
           ;; The next run already took `:abort' and 1/2/3 out of the queue
           ;; together as its batch, ahead of unwinding on `:abort'.
           (is (= 0 (queued-count msgbox-queue)))
           (setf abort-release t)
           (is-true (await-cond 2.0 (= 3 (length handled))))
           (is (equal '(1 2 3) (reverse handled)))
           (is (= 4 (ask-s actor 4 :time-out 1))))
      (ac:shutdown system))))

(test dispatch--stop-with-backlog--discards-items-and-wakes-ask-s-waiter
  "Stopping a message-box/dp while a batch is running discards the items still
queued behind the one being processed instead of handling them, and wakes an
`ask-s' caller waiting on one of those discarded items rather than leaving it
hanging."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 1 :throughput 10))))))
    (unwind-protect
         (let* ((release nil)
                (started nil)
                (processed 0)
                (actor (actor-of system
                                 :receive (lambda (msg)
                                            (when (eq msg :block)
                                              (setf started t)
                                              (loop :until release :do (sleep 0.01)))
                                            (incf processed))))
                (msgbox-queue (slot-value (act-cell:msgbox actor) 'mesgb::queue))
                (ask-result :not-set))
           (tell actor :block)
           (is-true (await-cond 0.5 started))
           (loop :repeat 3 :do (tell actor :go))
           (let ((ask-thread (bt2:make-thread
                              (lambda ()
                                (setf ask-result
                                      (handler-case
                                          (ask-s actor :go :time-out 2)
                                        (ask-timeout () :timeout)))))))
             ;; Wait until the ask-s item itself is queued behind the three
             ;; :go items before stopping, so the item under test is part of
             ;; the discarded backlog rather than raced against it.
             (is-true (await-cond 0.5 (= 4 (queued-count msgbox-queue))))
             (ac:stop system actor)
             (setf release t)
             (is-true (await-cond 1.0 (not (eq ask-result :not-set))))
             (bt2:join-thread ask-thread))
           (is (eql nil ask-result))
           (is (= 1 processed)))
      (ac:shutdown system))))

(test dispatch--throughput--from-dispatcher-config
  "The message-box takes its throughput from the dispatcher config."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 1 :throughput 3))))))
    (unwind-protect
         (let ((actor (actor-of system :receive #'identity)))
           (is (= 3 (disp:throughput (act-cell:msgbox actor)))))
      (ac:shutdown system))))

(test dispatch--throughput--explicit-on-message-box
  "A message-box created with an explicit `:throughput' keeps it over the
dispatcher's, and a throughput below 1 is rejected."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 1))))))
    (unwind-protect
         (let* ((dispatcher (getf (asys:dispatchers system) :shared))
                (msgbox (make-instance 'mesgb:message-box/dp
                                       :dispatcher dispatcher
                                       :throughput 7)))
           (is (= 7 (disp:throughput msgbox)))
           (signals error (make-instance 'mesgb:message-box/dp
                                         :dispatcher dispatcher
                                         :throughput 0)))
      (ac:shutdown system))))

(test dispatch/reply--with-timeout--no-poll-latency
  "A timed `ask-s' on the dispatcher message-box must return as soon as the
handler is done. The previous implementation polled the item in 50ms steps, so
50 calls took at least 2.5 seconds; waiting on the condition-variable finishes
them in a small fraction of that."
  (with-fixture dp-actor (#'identity)
    (let* ((start (get-internal-real-time))
           (results (loop :for i :from 0 :below 50
                          :collect (ask-s actor i :time-out 1)))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
      (is (equal (loop :for i :from 0 :below 50 :collect i) results))
      (is (< elapsed 1.0) "50 timed asks took ~a seconds" elapsed))))
