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

(defun make-test-dispatcher (num-workers)
  (make-dispatcher :num-workers num-workers))

(test create-dispatcher
  "Checks creating a dispatcher"
  (let ((cut (make-test-dispatcher 1)))
    (is (not (null cut)))
    (shutdown cut)))

(test create-the-workers
  "Checks that the workers are created as gservers"
  (let ((cut (make-test-dispatcher 4)))
    (is (= 4 (length (workers cut))))
    (shutdown cut)))

(test dispatch-to-worker
  "Tests the dispatching to a worker"
  (let ((cut (make-test-dispatcher 1)))
    (is (= 15 (dispatch cut (lambda () (loop :for i :from 1 :to 5 :sum i)))))
    (shutdown cut)))

(test shutdown-dispatcher
  "Tests shutting down a dispatcher and stopping all workers."
  (flet ((len-message-threads () (length
                                  (remove-if-not (lambda (x)
                                                   (str:starts-with-p "message-thread-mesgb" x))
                                                 (mapcar #'bt:thread-name (bt:all-threads))))))
    ;; make sure there are no other message-box threads
    (utils:wait-cond (lambda () (= 0 (len-message-threads))) 0.5 3)
    (let* ((len-message-threads-before (len-message-threads))
           (cut (make-test-dispatcher 4)))
      (mapcar (lambda (worker) (ask-s worker (cons :execute (lambda () )))) (workers cut))
      (is-true (assert-cond
                (lambda ()
                  (= (+ len-message-threads-before 4) (len-message-threads)))
                2))
      (shutdown cut)
      (is-true (assert-cond
                (lambda ()
                  (= len-message-threads-before (len-message-threads)))
                5)))))
