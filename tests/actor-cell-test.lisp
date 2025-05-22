(defpackage :sento.actor-cell-test
  (:use :cl :fiveam :sento.actor-cell)
  (:export #:run!
           #:all-tests
           #:nil)
  (:import-from #:miscutils
                #:await-cond))

(in-package :sento.actor-cell-test)

(def-suite actor-cell-tests
  :description "actor-cell tests"
  :in sento.tests:test-suite)

(in-suite actor-cell-tests)

(log:config :warn)

(defclass test-cell (actor-cell) ())

(def-fixture cell-fixture (call-fun cast-fun state)
  (defmethod handle-call ((cell test-cell) message)
    (funcall call-fun message))
  (defmethod handle-cast ((cell test-cell) message)
    (funcall cast-fun message))

  (let ((cut (make-instance 'test-cell
                            :state state)))
    (setf (msgbox cut) (make-instance 'mesgb:message-box/bt))
    (unwind-protect
         (&body)
      (progn
        (stop cut)
        (setf mesgb::*thread-destroy-waittime-s* 3.0) ;; restore orig
        ))))

(test get-cell-name-and-state
  "Just retrieves the name of the cell"
  (with-fixture cell-fixture (nil nil '(1))
    (is (= 0 (search "actor-" (name cut))))
    (is (equalp '(1) (state cut)))))

;; (test run-pre-start-fun
;;   "Tests the execution of `pre-start'"
;;   (with-fixture cell-fixture ((lambda (message)
;;                                 (declare (ignore message))
;;                                 *state*)
;;                               nil
;;                               (lambda (self)
;;                                 (setf *state* "pre-start-fun-executed"))
;;                               nil
;;                               0)
;;     (is (string= "pre-start-fun-executed" (call cut :foo)))))

;; (test run-after-stop-fun
;;   "Tests the execution of `after-stop'"
;;   (with-fixture cell-fixture ((lambda (message)
;;                                 (declare (ignore message)))
;;                               nil
;;                               nil
;;                               (lambda (self)
;;                                 (declare (ignore self))
;;                                 (setf *after-stop-val* "after-stop-fun-executed"))
;;                               0)
;;     (call cut :stop)
;;     (is (string= "after-stop-fun-executed" *after-stop-val*))))

(test no-message-box
  "Test responds with :no-message-handling when no msgbox is configured."
  (defclass no-msg-server (actor-cell) ())
  (defmethod handle-call ((cell no-msg-server) message)
    nil)
  (let ((cut (make-instance 'no-msg-server)))
    (is (eq :no-message-handling (call cut :foo)))))

(test handle-call
  "Simple cell handle-call test."
  (with-fixture cell-fixture ((lambda (message)
                                (cond
                                 ((and (listp message) (eq :add (car message)))
                                  (let ((new-state (+ *state* (cadr message))))
                                    (setf *state* new-state)
                                    new-state))
                                 ((and (listp message) (eq :sub (car message)))
                                  (let ((new-state (- *state* (cadr message))))
                                    (setf *state* new-state)
                                    new-state))
                                 ((and (listp message) (eq :explicit-return (car message)))
                                  (cadr message))))
                              nil
                              0)
    (is (= 1000 (call cut '(:add 1000))))
    (is (= 500 (call cut '(:sub 500))))
    (is (= 200 (call cut '(:explicit-return 200))))
    (is (eq nil (call cut "unhandled")))))


(defparameter *pong-received* nil)
(test cast--send-result-back-to-sender
  "Test that a cast, if implemented, can use `*sender*' to send result to 'sender'."
  (defclass pong-cell (actor-cell) ())
  (defmethod handle-cast ((cell pong-cell) message)
    (when (eq message :pong)
      (setf *pong-received* t)))
  (with-fixture cell-fixture (nil
                              (lambda (msg)
                                (case msg
                                  (:ping (cast *sender* :pong))))
                              0)
    (let ((fake-sender (make-instance 'pong-cell)))
      (setf (msgbox fake-sender) (make-instance 'mesgb:message-box/bt))
      (cast cut :ping fake-sender)
      (is-true (await-cond 0.5 *pong-received*))
      (stop fake-sender))))

(test error-in-handler
  "testing error handling"
  (with-fixture cell-fixture ((lambda (message)
                                (log:info "Raising error condition...")
                                (cond
                                 ((eq :err (car message))
                                  (error "Foo Error"))))
                              nil
                              nil)
    (let ((msg (call cut '(:err))))
      (format t "Got msg : ~a~%" msg)
      (is (not (null (cdr msg))))
      (is (eq (car msg) :handler-error))
      (is (string= "Foo Error" (format nil "~a" (cdr msg)))))))

(test stack-cell
  "a actor-cell as stack."
  (with-fixture cell-fixture ((lambda (message)
                                (cond
                                  ((eq :pop message)
                                   (let ((head (car *state*))
                                         (tail (cdr *state*)))
                                     (setf *state* tail)
                                     head))
                                  ((eq :get message)
                                   *state*)))
                              (lambda (message)
                                (cond
                                 ((eq :push (car message))
                                  (let ((new-state (append *state* (list (cdr message)))))
                                    (setf *state* new-state)))))
                              '(5))
    (is (equalp '(5) (call cut :get)))
    (cast cut (cons :push 4))
    (cast cut (cons :push 3))
    (cast cut (cons :push 2))
    (cast cut (cons :push 1))
    (is-true (await-cond 0.5 (equalp '(5 4 3 2 1) (call cut :get))))
    (is (= 5 (call cut :pop)))
    (is (= 4 (call cut :pop)))
    (is (= 3 (call cut :pop)))
    (is (= 2 (call cut :pop)))
    (is (= 1 (call cut :pop)))
    (is (null (call cut :pop)))))

(test stopping-cell--using-message
  "Stopping a cell stops the message handling and frees resources."
  (defclass stopping-cell (actor-cell) ())
  (defmethod handle-call ((cell stopping-cell) message)
    nil)
  (let ((cut (make-instance 'stopping-cell)))
    (setf (msgbox cut) (make-instance 'mesgb:message-box/bt))
    (is (eq :stopped (call cut :stop)))))

(test stopping-cell--using-stop
  "Stopping using `stop` method should discard any other enqueued messages."
  (let ((received nil))
    (with-fixture cell-fixture (nil
                                (lambda (message)
                                  (push message received)
                                  (sleep .2)
                                  nil)
                                0)
      (cast cut :first)
      (cast cut :second)
      (sleep 0.1) ;; make a small sleep to get at least the first message processed
      (stop cut)
      (cast cut :third)
      (sleep .5)
      (is (equalp '(:first) received))
      ))
  )

(test stopping-cell--using-terminate-message
  "Stopping using `:terminate` message behaves the same as using `stop' method."
  (let ((received nil))
    (with-fixture cell-fixture (nil
                                (lambda (message)
                                  (push message received)
                                  (sleep .2)
                                  nil)
                                0)
      (cast cut :first)
      (cast cut :second)
      (sleep 0.1) ;; make a small sleep to get at least the first message processed
      (cast cut :terminate)
      (cast cut :third)
      (sleep .5)
      (is (equalp '(:first) received))
      ))
  )

(test stopping-cell--using-stop--with-wait
  "Stopping using `stop` method should discard any other enqueued messages."
  (let ((received nil))
    (with-fixture cell-fixture (nil
                                (lambda (message)
                                  (push message received)
                                  (sleep .2)
                                  nil)
                                0)
      (cast cut :first)
      (cast cut :second)
      (sleep 0.1) ;; make a small sleep to get at least the first message processed
      (stop cut t)
      (cast cut :third)
      (is (equalp '(:first) received))
      ))
  )

(test stop--with-wait
  "Tests `stop` function of the cell."
  (with-fixture cell-fixture (nil
                              (lambda (message)
                                (declare (ignore message))
                                (sleep 0.3)
                                "receive-return")
                              0)
    (let ((start (timeutils:get-current-millis)))
      (declare (ignore start))
      (cast cut :foo)  ;; send message that waits a bit but is async
      (stop cut t)  ;; stop has to wait until stopped
      ;; TODO: check elapsed time
      ;; on a dispatcher messagebox stop with wait does nothing.
      (is-false (bt2:thread-alive-p (slot-value (msgbox cut) 'mesgb::queue-thread))))))

(test stop--with-wait--destroy-thread
  "Tests `stop` function of the cell on a message-box thread that is blocked and can't join."
  (with-fixture cell-fixture (nil
                              (lambda (message)
                                (declare (ignore message))
                                (sleep 20))
                              0)
    (setf mesgb::*thread-destroy-waittime-s* 1.0)  ;; wait 1 second until thread is destroyed
    (is-true (await-cond 1.0
               (bt2:thread-alive-p (slot-value (msgbox cut) 'mesgb::queue-thread))))
    (let ((start (timeutils:get-current-millis))
          (stop nil))
      (cast cut :foo)
      (stop cut t)
      (setf stop (timeutils:get-current-millis))
      (is-true (await-cond 0.5
                   (not (bt2:thread-alive-p (slot-value (msgbox cut) 'mesgb::queue-thread)))))
      (is (<= (- stop start) 1500)))))

(test stop--without-wait--destroy-thread
  "Tests `stop` function of the cell on a message-box thread that is blocked and can't join."
  (with-fixture cell-fixture (nil
                              (lambda (message)
                                (declare (ignore message))
                                (sleep 20))
                              0)
    (setf mesgb::*thread-destroy-waittime-s* 1.0)  ;; wait 1 second until thread is destroyed
    (is-true (await-cond 1.0
               (bt2:thread-alive-p (slot-value (msgbox cut) 'mesgb::queue-thread))))
    (let ((start (timeutils:get-current-millis))
          (stop nil))
      (cast cut :foo)
      (stop cut nil)
      (is-true (await-cond 1.5
                   (not (bt2:thread-alive-p (slot-value (msgbox cut) 'mesgb::queue-thread)))))
      (setf stop (timeutils:get-current-millis))
      (is (<= (- stop start) 1500)))))
