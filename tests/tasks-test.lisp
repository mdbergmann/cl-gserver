(defpackage :cl-gserver.tasks-test
  (:use :cl :fiveam :cl-gserver.utils :cl-gserver.tasks)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.tasks-test)

(def-fixture system-fixture ()
  ;; creates an additional dispatcher called FOO
  (let ((system (asys:make-actor-system '(:dispatchers (:foo (:workers 1))))))
    (unwind-protect
         (let ((initial-system-actors (ac:all-actors system)))
           (&body)
           (assert-cond (lambda ()
                          (= (length (ac:all-actors system))
                             (length initial-system-actors)))
                        0.5))
      (ac:shutdown system))))

(def-suite tasks-tests
  :description "Task tests"
  :in cl-gserver.tests:test-suite)

(in-suite tasks-tests)

(test task-yield
  "Tests for task-yield"
  (with-fixture system-fixture ()
      (with-context (system)
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
    (with-context (system)
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
    (with-context (system)
      (let ((task (task-async (lambda () (+ 1 2)))))
        (is-true (typep task 'tasks:task))
        (task-shutdown task)))))

(test task-async--completion-handler-and-await
  "Test for task-async passed result via completion halder."
  (with-fixture system-fixture ()
    (with-context (system)
      (let* ((completion-result)
             (completion-handler (lambda (result) (setf completion-result result)))
             (task (task-async (lambda () (+ 1 2))
                               :on-complete-fun completion-handler)))
        (is (assert-cond (lambda () (and (not (null completion-result))
                                    (= completion-result 3))) .5))
        (is (= 3 (task-await task)))))))

(test task-async--completion-handler--err-cond
  "Test for task-async passed result via completion handler, error on handler."
  (with-fixture system-fixture ()
    (with-context (system)
      (let* ((completion-result)
             (completion-handler (lambda (result) (setf completion-result result))))
        (task-async (lambda () (error "Foo"))
                    :on-complete-fun completion-handler)
        (is (assert-cond (lambda () (and (not (null completion-result))
                                    (eq :handler-error (car completion-result)))) .5))))))

(test task-async--in-custom-dispatcher
  "Test for task-async in custom dispatcher."
  (with-fixture system-fixture ()
    (with-context (system :foo)
      (let ((task (task-async (lambda () (+ 1 2)))))
        (is-true (typep task 'tasks:task))
        (is (eq :foo (slot-value (mesgb::dispatcher (act-cell:msgbox task)) 'disp::identifier)))
        (task-shutdown task)))))

(test task-async--with-await
  "Test for task-async followed by task-await."
  (with-fixture system-fixture ()
    (with-context (system)
      (let ((task (task-async (lambda () (+ 1 2)))))
        (is (= 3 (task-await task)))))))

(test task-async--with-await--err-cond
  "Test for task-async followed by task-await with error condition."
  (with-fixture system-fixture ()
    (with-context (system)
      (let ((task (task-async (lambda () (error "Foo")))))
        (is (eq :handler-error (car (task-await task))))))))

(test task-async--with-await--longer-wait
  "Test for task-async followed by task-await."
  (with-fixture system-fixture ()
    (with-context (system)
      (let ((task (task-async (lambda () (sleep 1) (+ 1 2)))))
        (is (= 3 (task-await task)))))))

(test task-async--with-await--timeout
  "Test for task-async followed by task-await, timeout raised."
  (with-fixture system-fixture ()
    (with-context (system)
      (let* ((task (task-async (lambda () (sleep 1) (+ 1 2))))
             (await-result (task-await task 0.5)))
        (is (eq :handler-error (car await-result)))
        (is (typep (cdr await-result) 'ask-timeout))))))

(test task-async-stream
  "Tests for task-async-stream"
  (with-fixture system-fixture ()
    (with-context (system)
      (is (equal '(2 4 6 8 10)
                 (task-async-stream (lambda (x) (* x 2))
                                    '(1 2 3 4 5))))
      (is (= 30 (reduce #'+
                        (task-async-stream (lambda (x) (* x 2))
                                           '(1 2 3 4 5))))))))

(test task-async-stream--with-err-results
  "Tests for task-async-stream where computations contains errors."
  (with-fixture system-fixture ()
    (with-context (system)
      (is (= 24 (reduce #'+
                        (utils:filter (lambda (x) (not (consp x)))
                                      (task-async-stream (lambda (x) (* x 2))
                                                         '(1 2 "f" 4 5)))))))))
