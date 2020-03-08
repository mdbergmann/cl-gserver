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

(log:config :info)

(init-dispatcher-threadpool 1)

(test get-server-name
  "Just retrieves the name of the server"

  (let ((server (make-instance 'gserver)))
    (is (= 0 (search "Server-" (name server))))))


(test handle-call
  "Simple add-server handle-call test."

  (defclass add-server (gserver) ())
  (defmethod handle-call ((server add-server) message current-state)
    (match message
      ((list :add n)
       (let ((new-state (+ current-state n)))
         (cons new-state new-state)))
      ((list :sub n)
       (let ((new-state (- current-state n)))
         (cons new-state new-state)))))

  (let ((cut (make-instance 'add-server :state 0)))
    (is (= 1000 (call cut '(:add 1000))))
    (is (= 500 (call cut '(:sub 500))))
    (is (eq :unhandled (call cut "Foo")))))


(test error-in-handler
  "testing error handling"
  
  (defclass err-server (gserver) ())
  (defmethod handle-call ((server err-server) message current-state)
    (match message
      ((list :err) (/ 5 0))))  ; division by 0

  (let* ((cut (make-instance 'err-server))
         (result (call cut '(:err))))
    (format t "Got result : ~a~%" result)
    (is (not (null (cdr result))))
    (is (eq (car result) :handler-error))))


(test stack-server
  "a gserver as stack."

  (defclass stack-server (gserver) ())
  (defmethod handle-call ((server stack-server) message current-state)
    (log:debug "current-state: " current-state)
    (match message
      (:pop
       (cons
        (car current-state)
        (cdr current-state)))
      (:get
       (cons current-state current-state))))
  (defmethod handle-cast ((server stack-server) message current-state)
    (log:debug "current-state: " current-state)
    (match message
      ((cons :push value)
       (let ((new-state (append current-state (list value))))
         (cons new-state new-state)))))

  (let ((cut (make-instance 'stack-server :state '(5))))
    (is (equalp '(5) (call cut :get)))
    (cast cut (cons :push 4))
    (is (equalp '(5 4) (call cut :get)))
    (cast cut (cons :push 3))
    (is (equalp '(5 4 3) (call cut :get)))
    (cast cut (cons :push 2))
    (is (equalp '(5 4 3 2) (call cut :get)))
    (cast cut (cons :push 1))
    (is (equalp '(5 4 3 2 1) (call cut :get)))
    (sleep 0.5)
    (is (= 5 (call cut :pop)))
    (is (= 4 (call cut :pop)))
    (is (= 3 (call cut :pop)))
    (is (= 2 (call cut :pop)))
    (is (= 1 (call cut :pop)))
    (is (null (call cut :pop)))
    ))


(run! 'get-server-name)
(run! 'handle-call)
(run! 'error-in-handler)
(run! 'stack-server)
