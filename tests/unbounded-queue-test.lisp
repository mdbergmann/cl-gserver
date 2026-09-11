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

(test unbounded-queue--try-popq
  "`try-popq' returns the element and `T' when one is queued, `nil' and `nil'
without blocking when the queue is empty."
  (let ((cut (make-instance 'queue-unbounded)))
    (is (equal '(nil nil) (multiple-value-list (try-popq cut))))
    (pushq cut 1)
    (pushq cut 2)
    (is (equal '(1 t) (multiple-value-list (try-popq cut))))
    (is (= 1 (queued-count cut)))
    (is (equal '(2 t) (multiple-value-list (try-popq cut))))
    (is (= 0 (queued-count cut)))
    (is (equal '(nil nil) (multiple-value-list (try-popq cut))))
    (is-true (emptyq-p cut))))

(test unbounded-queue--push-pop
  "Pushes and pops an element and checks the count and emptiness on the way."
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
