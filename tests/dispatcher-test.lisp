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
  (let ((context (asys:make-actor-system '(:dispatchers (:shared (:workers 0))))))
    (unwind-protect
         (&body)
      (ac:shutdown context))))


(defun make-test-dispatcher (num-workers context ident)
  (make-dispatcher (ac:make-actor-context context)
                   ident
                   :workers num-workers))

(test create-dispatcher
  "Checks creating a dispatcher"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 1 context "foo")))
      (is (not (null cut)))
      (is (string= "foo" (identifier cut)))
      ;;(is (str:containsp "RANDOM-STRATEGY" (format nil "disp: ~a~%" cut)))
      (stop cut))))

(test create-dispatcher--with-config
  "Tests creating a dispatcher with a custom config."
  (with-fixture test-context ()
    (let ((cut (apply #'make-dispatcher context :foo '(:workers 0 :strategy :round-robin))))
      (is (eq :foo (identifier cut)))
      (is (= 0 (length (workers cut))))
      ;;(is (str:containsp "ROUND-ROBIN" (format nil "disp: ~a~%" cut)))
      (stop cut))))

(test create-the-workers
  "Checks that the workers are created as actors"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 4 context "foo")))
      (is (= 4 (length (workers cut))))
      (stop cut))))

(test dispatch-to-worker
  "Tests the dispatching to a worker"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 1 context "foo")))
      (is (= 15 (dispatch cut (lambda () (loop :for i :from 1 :to 5 :sum i)))))
      (stop cut))))
