(defpackage :cl-gserver.agent.array-test
  (:use :cl :fiveam :cl-gserver.agent.array)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.agent.array-test)

(def-suite agent.array-tests
  :description "Tests for array agent"
  :in cl-gserver.tests:test-suite)

(in-suite agent.array-tests)

(def-fixture asys-fixture ()
  (let ((asys (asys:make-actor-system '(:dispatchers (:shared (:workers 1))))))
    (unwind-protect
         (&body)
      (ac:shutdown asys))))

(def-fixture agt ()
  (let ((cut (make-array-agent nil)))
    (unwind-protect
         (&body)
      (agt:agent-stop cut))))

(test create
  "Tests creating a array agent."
  (let ((cut (make-array-agent nil :initial-array #())))
    (is-true cut)
    (agt:agent-stop cut)))

(test create--in-system
  "Tests creating a array agent with providing an actor-context."
  (with-fixture asys-fixture ()
    (let ((cut (make-array-agent asys)))
      (is-true cut))))
