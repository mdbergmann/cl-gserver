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

(init-threadpool 1)

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
         (cons new-state new-state)))
      ((list :err) (/ 5 0))))  ; division by 0

  (let ((cut (make-instance 'add-server :state 0)))
    (is (= 1000 (call cut '(:add 1000))))
    (is (= 500 (call cut '(:sub 500))))
    (is (eq :unhandled (call cut "Foo")))))

(test error-in-handler
  
  (defclass err-server (gserver) ())
  (defmethod handle-call ((server err-server) message current-state)
    (match message
      ((list :err) (/ 5 0))))  ; division by 0

  (let* ((cut (make-instance 'err-server :state 0))
         (result (call cut '(:err))))
    (is (eq :handler-error (car result)))
    (is (not (null (typep (cdr result) 'division-by-zero))))))

(test stack-server

  (defclass stack-server (gserver) ())
  (defmethod handle-call ((server stack-server) message current-state)
    (match message
      (:pop (cons (car current-state) (cdr current-state)))))

  (let ((cut (make-instance 'stack-server :state '())))
    (is (null (call cut :pop))))

  ;; TODO: Expand on this
  )

(run! 'get-server-name)
(run! 'handle-call)
(run! 'error-in-handler)
(run! 'stack-server)
