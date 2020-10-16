(defpackage :cl-gserver.dispatcher-test
  (:use :cl :fiveam :cl-gserver.dispatcher-api :cl-gserver.dispatcher)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.dispatcher-test)

(def-suite dispatcher-tests
  :description "Tests for dispatcher"
  :in cl-gserver.tests:test-suite)

(in-suite dispatcher-tests)

(test create-dispatcher
  "Checks creating a dispatcher"
  (let ((cut (make-dispatcher 'dispatcher-bt)))
    (is (not (null cut)))
    (shutdown cut)))

(test create-the-workers
  "Checks that the workers are created as gservers"
  (let ((cut (make-dispatcher 'dispatcher-bt :num-workers 4)))
    (is (= 4 (length (workers cut))))
    (shutdown cut)))

(test dispatch-to-worker
  "Tests the dispatching to a worker"
  (let ((cut (make-dispatcher 'dispatcher-bt)))
    (is (= 15 (dispatch cut (lambda () (loop for i from 1 to 5 sum i)))))
    (shutdown cut)))

(defun runtests ()
  (run! 'create-dispatcher)
  (run! 'create-the-workers)
  (run! 'dispatch-to-worker))
