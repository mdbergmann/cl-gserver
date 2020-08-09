(defpackage :cl-gserver.gserver-test
  (:use :cl :trivia :fiveam :cl-gserver)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.gserver-test)

(def-suite gserver-tests
  :description "gserver tests"
  :in cl-gserver.tests:test-suite)

(in-suite gserver-tests)

(log:config :warn)

(def-fixture server-fixture (call-fun cast-fun state)
  (defclass test-server (gserver) ())
  (defmethod handle-call ((server test-server) message current-state)
    (funcall call-fun server message current-state))
  (defmethod handle-cast ((server test-server) message current-state)
    (funcall cast-fun server message current-state))

  (let ((cut (make-instance 'test-server :state state)))  
    (&body)
    (call cut :stop)))

(test get-server-name
  "Just retrieves the name of the server"

  (with-fixture server-fixture (nil nil nil)
    (is (= 0 (search "Server-" (name cut))))))


(test handle-call
  "Simple server handle-call test."

  (with-fixture server-fixture ((lambda (server message current-state)
                                  (declare (ignore server))
                                  (match message
                                    ((list :add n)
                                     (let ((new-state (+ current-state n)))
                                       (cons new-state new-state)))
                                    ((list :sub n)
                                     (let ((new-state (- current-state n)))
                                       (cons new-state new-state)))))
                                nil
                                0)
    (is (= 1000 (call cut '(:add 1000))))
    (is (= 500 (call cut '(:sub 500))))
    (is (eq :unhandled (call cut "Foo")))))

(test error-in-handler
  "testing error handling"
  
  (with-fixture server-fixture ((lambda (server message current-state)
                                  (declare (ignore server current-state))
                                  (log:debug "ERROR: error-in-handler message handler.")
                                  (match message
                                    ((list :err) (error "Foo Error"))))
                                nil
                                nil)
  (let ((result (call cut '(:err))))
    (format t "Got result : ~a~%" result)
    (is (not (null (cdr result))))
    (is (eq (car result) :handler-error))
    (is (string= "Foo Error" (format nil "~a" (cdr result)))))))


(test stack-server
  "a gserver as stack."

  (with-fixture server-fixture ((lambda (server message current-state)
                                  (declare (ignore server))
                                  (format t "current-state: ~a~%" current-state)
                                  (match message
                                    (:pop
                                     (cons
                                      (car current-state)
                                      (cdr current-state)))
                                    (:get
                                     (cons current-state current-state))))
                                (lambda (server message current-state)
                                  (declare (ignore server))
                                  (format t "current-state: ~a~%" current-state)
                                  (match message
                                    ((cons :push value)
                                     (let ((new-state (append current-state (list value))))
                                       (cons new-state new-state)))))
                                '(5))
    (is (equalp '(5) (call cut :get)))
    (cast cut (cons :push 4))
    (cast cut (cons :push 3))
    (cast cut (cons :push 2))
    (cast cut (cons :push 1))
    (sleep 0.5)
    (is (equalp '(5 4 3 2 1) (call cut :get)))
    (is (= 5 (call cut :pop)))
    (is (= 4 (call cut :pop)))
    (is (= 3 (call cut :pop)))
    (is (= 2 (call cut :pop)))
    (is (= 1 (call cut :pop)))
    (is (null (call cut :pop)))))

(test stopping-server
  "Stopping a server stops the message handling and frees resources."

  (defclass stopping-server (gserver) ())
  (defmethod handle-call ((server stopping-server) message current-state)
    (cons message current-state))

  (let ((cut (make-instance 'stopping-server)))
    (is (eq :stopped (call cut :stop)))))

(defun run-tests ()
  (run! 'get-server-name)
  (run! 'handle-call)
  (run! 'error-in-handler)
  (run! 'stack-server)
  (run! 'stopping-server))
