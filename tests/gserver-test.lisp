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

;; your test code here

(test get-server-name
  "Just retrieves the name of the server"

  (let ((server (make-instance 'gserver)))
    (is (= 0 (search "Server-" (name server))))))

(test simple-add-server
  "Creates a simple server which adds just a 1 to the number that is being sent."

  (init-threadpool 1)

  (defclass add-server (gserver) ())
  (defmethod handle-call ((server add-server) message)
    (match message
      ((list :add n) (cons :ok (1+ n)))
      ((list :sub n) (cons :ok (1- n)))
      ((list :err) (/ 5 0))))  ; division by 0

  (defun add-test (message)
    (let ((cut (make-instance 'add-server)))
      (call cut message)))

  (let ((result (add-test '(:add 1000))))
    (is (eq :ok (car result)))
    (is (= 1001 (cdr result))))

  (let ((result (add-test '(:sub 1000))))
    (is (eq :ok (car result)))
    (is (= 999 (cdr result))))

  (let ((result (add-test "Foo")))
    (is (eq :unhandled (car result)))
    (is (string= (cdr result) "")))

  ;; provoke raising an error in handle-call
  (let ((result (add-test '(:err))))
    (is (eq :handler-error (car result)))
    (is (not (null (typep (cdr result) 'division-by-zero)))))
  )

(run! 'get-server-name)
(run! 'simple-add-server)
