(defpackage :cl-gserver.tasks-test
  (:use :cl :fiveam :cl-gserver.utils :cl-gserver.tasks)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.tasks-test)

(def-fixture system-fixture ()
  (let ((system (asys:make-actor-system)))
    (unwind-protect
         (&body)
      (ac:shutdown system))))

(def-suite tasks-tests
  :description "Task tests"
  :in cl-gserver.tests:test-suite)

(in-suite tasks-tests)

(test task-yield
  "Tests for task-yield"
  (with-fixture system-fixture ()
    (with-context system
      (is (eq :foo (task-yield (lambda () :foo))))
      (is (= 10 (task-yield (lambda () (+ 5 5)))))
      (let ((timed-result (task-yield (lambda () (sleep .5)) 0.2)))
        (is (eq :handler-error (car timed-result)))
        (is (typep (cdr timed-result) 'ask-timeout))))))
