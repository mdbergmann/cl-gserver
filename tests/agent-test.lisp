(defpackage :cl-gserver.agent-test
  (:use :cl :trivia :fiveam :cl-gserver.agent)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.agent-test)

(def-suite agent-tests
  :description "agent tests"
  :in cl-gserver.tests:test-suite)

(in-suite agent-tests)

(log:config :info)

(test create-agent
  "Creates an agent"

  (let ((agent (make-agent (lambda () 0))))
    (is (not (null agent)))
    (agent-stop agent)))

(test get-agent-state
  "Gets agent state"

  (let ((agent (make-agent (lambda () '(5 4 3)))))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))
    (agent-stop agent)))

(test update-agent-state
  "Updates agent state"

  (let ((agent (make-agent (lambda () '(5 4 3)))))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))
    (is (eq t (agent-update agent (lambda (state) (mapcar #'1+ state)))))
    (sleep 0.5)
    (is (equalp '(6 5 4) (agent-get agent #'identity)))
    (agent-stop agent)))

(test stop-agent
  "Stop agent to cleanup resources."

  (let ((agent (make-agent (lambda () nil))))
    (is (eq :stopped (agent-stop agent)))))

(run! 'create-agent)
(run! 'get-agent-state)
(run! 'update-agent-state)
(run! 'stop-agent)
