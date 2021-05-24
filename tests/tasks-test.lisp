(defpackage :cl-gserver.tasks-test
  (:use :cl :fiveam :cl-gserver.utils :cl-gserver.tasks)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.tasks-test)

(def-fixture system-fixture ()
  (let ((system (asys:make-actor-system)))
    (unwind-protect
         (let ((initial-system-actors (ac:all-actors system)))
           (&body)
           (assert (= (length (ac:all-actors system))
                      (length initial-system-actors))))
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
        (is (equal '(2 4 6 8)
                   (mapcar (lambda (x)
                             (task-yield (lambda () (* 2 x))))
                           '(1 2 3 4))))
        (let ((timed-result (task-yield (lambda () (sleep .5)) 0.2)))
          (is (eq :handler-error (car timed-result)))
          (is (typep (cdr timed-result) 'ask-timeout))))))

(test task-start
  "Test for task-start"
  (with-fixture system-fixture ()
    (with-context system
      (let* ((my-var nil))
        (multiple-value-bind (result actor)
            (task-start (lambda () (setf my-var 10)))
          (is (eq :ok result))
          (is-true (typep actor 'act:actor))
          (is-true (assert-cond (lambda () (= 10 my-var)) 0.2)))))))
