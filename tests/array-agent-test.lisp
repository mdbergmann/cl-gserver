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

(def-fixture agt (arr)
  (let ((cut (make-array-agent nil :initial-array arr)))
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
    (let ((cut (make-array-agent asys :initial-array #())))
      (is-true cut))))

(test agent-elt
  "Tests retrieve element."
  (with-fixture agt (#(10 20))
    (is (= 10 (agent-elt 0 cut)))
    (is (= 20 (agent-elt 1 cut)))))

(test agent-push
  "Tests pushing new value."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t))
    (is-true (agent-push 1 cut))
    (is-true (agent-push 2 cut))
    (is (= 1 (agent-elt 0 cut)))
    (is (= 2 (agent-elt 1 cut)))))

(test agent-push-and-getidx
  "Tests pushing with returning the new index."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t))
    (is (= 0 (agent-push-and-getidx 1 cut)))
    (is (= 1 (agent-push-and-getidx 2 cut)))))

(test agent-push-and-getidx--err
  "Tests pushing with returning an error. Missing fill-pointer here."
  (with-fixture agt ((make-array 0 :adjustable t))
    (is (typep (agent-push-and-getidx 1 cut) 'error))))

(test agent-pop
  "Tests poping value."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t))
    (is-true (agent-push 1 cut))
    (is-true (agent-push 2 cut))
    (is (= 2 (agent-pop cut)))
    (is (= 1 (agent-pop cut)))))
