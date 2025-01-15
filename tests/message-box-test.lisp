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
      (loop while (bt2:thread-alive-p queue-thread)
            do (sleep 0.1)
               (when (< wait-until
                        (get-internal-real-time))
                 (error "Thread didn't die in ~A seconds."
                        timeout))))))


(test bt-box-resurrects-thread-after-error
  "Tests that if an error happends during message processing, a thread will remain running."

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
                           (list (lambda (msg)
                                   (handler-bind ((serious-condition #'abort))
                                     (error "Die, thread, die!")))))))
             (is (equal first-reply
                        'no-result)))

           (wait-while-thread-will-die box)

           (let ((result (handler-case
                             (submit box "The Message" t 1
                                     (list (lambda (msg)
                                             (reverse msg))))
                           (ask-timeout ()
                             :timeout))))
             (is (string= "egasseM ehT" result))))
      
      ;; Cleanup a thread:
      (stop box t))))
