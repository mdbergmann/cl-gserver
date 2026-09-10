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
