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
        (multiple-value-bind (result condition)
            (task-yield (lambda () (sleep .5)) 0.2)
          (format t "Cond: ~a~%" condition)
          (is (eq :handler-error result))
          (is-true (typep condition 'ask-timeout))))))

(test task-start
  "Test for task-start"
  (with-fixture system-fixture ()
    (with-context system
      (let ((my-var nil))
        (multiple-value-bind (result task)
            (task-start (lambda () (setf my-var 10)))
          (is (eq :ok result))
          (is-true (typep task 'tasks:task))
          (is-true (assert-cond (lambda () (= 10 my-var)) 0.2))
          (is (eq :stopped (act:ask-s task :foo))))))))

(test task-async
  "Test for task-async."
  (with-fixture system-fixture ()
    (with-context system
      (let ((task (task-async (lambda () (+ 1 2)))))
        (is-true (typep task 'tasks:task))
        (task-shutdown task)))))

(test task-async--with-await
  "Test for task-async followed by task-await."
  (with-fixture system-fixture ()
    (with-context system
      (let ((task (task-async (lambda () (+ 1 2)))))
        (is (= 3 (task-await task)))))))

(test task-async--with-await--err-cond
  "Test for task-async followed by task-await with error condition."
  (with-fixture system-fixture ()
    (with-context system
      (let ((task (task-async (lambda () (error "Foo")))))
        (is (eq :handler-error (car (task-await task))))))))

(test task-async--with-await--longer-wait
  "Test for task-async followed by task-await."
  (with-fixture system-fixture ()
    (with-context system
      (let ((task (task-async (lambda () (sleep 1) (+ 1 2)))))
        (is (= 3 (task-await task)))))))

(test task-async-stream
  "Tests for task-async-stream"
  (with-fixture system-fixture ()
    (with-context system
      (is (equal '(2 4 6 8 10)
                 (task-async-stream (lambda (x) (* x 2))
                                    '(1 2 3 4 5))))
      (is (= 30 (reduce #'+
                        (task-async-stream (lambda (x) (* x 2))
                                           '(1 2 3 4 5))))))))

(test task-async-stream--with-err-results
  "Tests for task-async-stream"
  (with-fixture system-fixture ()
    (with-context system
      (is (equal '(2 4 6 8 10)
                 (task-async-stream (lambda (x) (* x 2))
                                    '(1 2 3 4 5))))
      (is (= 30 (reduce #'+
                        (task-async-stream (lambda (x) (* x 2))
                                           '(1 2 3 4 5))))))))
