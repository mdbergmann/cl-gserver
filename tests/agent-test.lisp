(defpackage :cl-gserver.agent-test
  (:use :cl :trivia :fiveam :cl-gserver.agent)
  (:import-from #:utils
                #:assert-cond)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.agent-test)

(def-suite agent-tests
  :description "agent tests"
  :in cl-gserver.tests:test-suite)

(in-suite agent-tests)

(log:config :warn)

(def-fixture agent-fixture (fun)
  (let ((agent (make-agent (lambda () (funcall fun)))))
    (&body)
    (agent-stop agent)))


(test create-agent
  "Creates an agent"
  (with-fixture agent-fixture ((lambda () 0))
    (is (not (null agent)))))


(test get-agent-state
  "Gets agent state"
  (with-fixture agent-fixture ((lambda () '(5 4 3)))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))))


(test update-agent-state
  "Updates agent state"
  (with-fixture agent-fixture ((lambda () '(5 4 3)))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))
    (is (eq t (agent-update agent (lambda (state) (mapcar #'1+ state)))))
    (is (eq t (assert-cond (lambda ()
                             (equalp '(6 5 4) (agent-get agent #'identity)))
                           1)))))


(test stop-agent
  "Stop agent to cleanup resources."
  (let ((agent (make-agent (lambda () nil))))
    (agent-stop agent)
    (is (eq t (assert-cond (lambda ()
                             (eq :stopped (agent-get agent #'identity)))
                           1)))))

(defun run-tests ()
  (run! 'create-agent)
  (run! 'get-agent-state)
  (run! 'update-agent-state)
  (run! 'stop-agent))
