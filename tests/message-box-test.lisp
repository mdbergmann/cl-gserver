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
                #:no-result
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
   Usually this kill a thread, but here we ensure that by the thread is resurrected when we submit a
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
                     (submit box "The Message"
                             t
                             ;; Don't wait for result here, because we are
                             ;; intentionally raise error here and will never
                             ;; return a result:
                             nil
                             (list #'kill-by-restart-invoke))))
               (is (equal first-reply
                          'no-result)))

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
