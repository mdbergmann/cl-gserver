(defpackage :sento.unbounded-queue-test
  (:use :cl :fiveam :sento.queue)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.unbounded-queue-test)

(def-suite unbounded-queue-tests
  :description "Tests unbounded queue"
  :in sento.tests:test-suite)

(in-suite unbounded-queue-tests)

(test unbounded-queue--push-pop
  (let ((cut (make-instance 'queue-unbounded)))
    (pushq cut 1)
    (is-false (emptyq-p cut))
    (is (= (queued-count cut) 1))
    (is (= 1 (popq cut)))
    (is (= (queued-count cut) 0))
    (is-true (emptyq-p cut))
    (pushq cut 1)
    (is (= (queued-count cut) 1))
    ))
