(defpackage :sento.bounded-queue-test
  (:use :cl :fiveam :sento.queue)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.bounded-queue-test)

(def-suite bounded-queue-tests
  :description "Tests bounded queue"
  :in sento.tests:test-suite)

(in-suite bounded-queue-tests)

(test bounded-queue--push-pop
  (let ((cut (make-instance 'queue-bounded :max-items 2)))
    (pushq cut 1)
    (is-false (emptyq-p cut))
    (is (= 1 (popq cut)))
    (is-true (emptyq-p cut))))

(test bounded-queue--try-popq
  "`try-popq' returns the element and `T' when one is queued, `nil' and `nil'
without blocking when the queue is empty, and frees a slot of the bound."
  (let ((cut (make-instance 'queue-bounded :max-items 2)))
    (is (equal '(nil nil) (multiple-value-list (try-popq cut))))
    (pushq cut 1)
    (pushq cut 2)
    (is (equal '(1 t) (multiple-value-list (try-popq cut))))
    (is (= 1 (queued-count cut)))
    (pushq cut 3)
    (is (equal '(2 t) (multiple-value-list (try-popq cut))))
    (is (equal '(3 t) (multiple-value-list (try-popq cut))))
    (is (equal '(nil nil) (multiple-value-list (try-popq cut))))
    (is-true (emptyq-p cut))))

(test bounded-queue--raise-condition-when-queue-full
  (let ((cut (make-instance 'queue-bounded :max-items 2)))
    (pushq cut 1)
    (pushq cut 2)
    (is (= 2 (queued-count cut)))
    (signals queue-full-error (pushq cut 3))
    (is (= 1 (popq cut)))
    (is (= 1 (queued-count cut)))
    (pushq cut 3)))
