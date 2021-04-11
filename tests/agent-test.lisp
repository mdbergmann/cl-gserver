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

(def-fixture agent-asys-fixture (fun)
  (let ((asys (asys:make-actor-system '(:dispatchers (:num-shared-workers 1)))))
    (unwind-protect
         (let ((agent (make-agent (lambda () (funcall fun)) asys)))
           (&body))
      (ac:shutdown asys))))

(test create-agent
  "Creates an agent"
  (with-fixture agent-fixture ((lambda () 0))
    (is (not (null agent)))))

(test create-agent--on-system
  "Creates an agent on a system, which implies using a shared dispatcher."
  (with-fixture agent-asys-fixture ((lambda () 0))
    (is (not (null agent)))
    (is (typep (act-cell:msgbox agent) 'mesgb:message-box/dp))))


(test get-agent-state
  "Gets agent state"
  (with-fixture agent-fixture ((lambda () '(5 4 3)))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))))

(test get-agent-state--on-system
  "Gets agent state - with agent on system."
  (with-fixture agent-asys-fixture ((lambda () '(5 4 3)))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))))


(test update-agent-state
  "Updates agent state"
  (with-fixture agent-fixture ((lambda () '(5 4 3)))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))
    (is (agent-update agent (lambda (state) (mapcar #'1+ state))))
    (is (assert-cond (lambda ()
                       (equalp '(6 5 4) (agent-get agent #'identity)))
                     1))))

(test update-agent-state--on-system
  "Updates agent state - with agent on system."
  (with-fixture agent-asys-fixture ((lambda () '(5 4 3)))
    (is (equalp '(5 4 3) (agent-get agent #'identity)))
    (is (agent-update agent (lambda (state) (mapcar #'1+ state))))
    (is (assert-cond (lambda ()
                       (equalp '(6 5 4) (agent-get agent #'identity)))
                     1))))


(test stop-agent
  "Stop agent to cleanup resources."
  (let ((agent (make-agent (lambda () nil))))
    (agent-stop agent)
    (is (assert-cond (lambda ()
                       (eq :stopped (agent-get agent #'identity)))
                     1))))

(test stop-agent--on-system
  "Stop agent to cleanup resources - with agent on system."
  (with-fixture agent-asys-fixture ((lambda () '(5 4 3)))
    (agent-stop agent)
    (is (assert-cond (lambda ()
                       (eq :stopped (agent-get agent #'identity)))
                     1))))


(defun run-tests ()
  (run! 'create-agent)
  (run! 'create-agent--on-system)
  (run! 'get-agent-state)
  (run! 'get-agent-state--on-system)
  (run! 'update-agent-state)
  (run! 'update-agent-state--on-system)
  (run! 'stop-agent)
  (run! 'stop-agent--on-system))
