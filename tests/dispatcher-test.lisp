(defpackage :cl-gserver.dispatcher-test
  (:use :cl :fiveam :cl-gserver.dispatcher
        :cl-gserver.actor)
  (:import-from #:utils
                #:assert-cond)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.dispatcher-test)

(def-suite dispatcher-tests
  :description "Tests for dispatcher"
  :in cl-gserver.tests:test-suite)

(in-suite dispatcher-tests)

(def-fixture test-context ()
  (let ((context (asys:make-actor-system '(:dispatchers (:num-shared-workers 0)))))
    (unwind-protect
         (&body)
      (ac:shutdown context)
      (sleep 0.2))))


(defun make-test-dispatcher (num-workers context)
  (make-dispatcher (ac:make-actor-context context) :num-workers num-workers))

(test create-dispatcher
  "Checks creating a dispatcher"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 1 context)))
      (is (not (null cut)))
      (shutdown cut))))

(test create-the-workers
  "Checks that the workers are created as actors"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 4 context)))
      (is (= 4 (length (workers cut))))
      (shutdown cut))))

(test dispatch-to-worker
  "Tests the dispatching to a worker"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 1 context)))
      (is (= 15 (dispatch cut (lambda () (loop :for i :from 1 :to 5 :sum i)))))
      (shutdown cut))))
