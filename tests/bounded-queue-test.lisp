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

(test bounded-queue--try-popq-into
  "`try-popq-into' pops up to the vector's length elements into the vector,
returns how many, 0 on an empty queue, and frees that many slots of the bound."
  (let ((cut (make-instance 'queue-bounded :max-items 5))
        (batch (make-array 3 :initial-element nil)))
    (is (= 0 (try-popq-into cut batch)))
    (loop :for i :from 1 :to 5 :do (pushq cut i))
    (signals queue-full-error (pushq cut 6))
    (is (= 3 (try-popq-into cut batch)))
    (is (equalp #(1 2 3) batch))
    (is (= 2 (queued-count cut)))
    (pushq cut 6)
    (is (= 3 (try-popq-into cut batch)))
    (is (equalp #(4 5 6) batch))
    (is-true (emptyq-p cut))
    (is (= 0 (try-popq-into cut batch)))))

(test bounded-queue--popq--blocks-until-push
  "A `popq' on an empty queue blocks the calling thread and is woken by a
`pushq'. The waiter count the push consults is back at 0 afterwards."
  (let* ((cut (make-instance 'queue-bounded :max-items 2))
         (result :none)
         (popper (bt2:make-thread (lambda () (setf result (popq cut))))))
    (is-true (miscutils:await-cond 0.5 (= 1 (slot-value cut 'sento.queue::waiters))))
    (is (eq :none result))
    (pushq cut :woken)
    (bt2:join-thread popper)
    (is (eq :woken result))
    (is (= 0 (slot-value cut 'sento.queue::waiters)))))

(test bounded-queue--raise-condition-when-queue-full
  (let ((cut (make-instance 'queue-bounded :max-items 2)))
    (pushq cut 1)
    (pushq cut 2)
    (is (= 2 (queued-count cut)))
    (signals queue-full-error (pushq cut 3))
    (is (= 1 (popq cut)))
    (is (= 1 (queued-count cut)))
    (pushq cut 3)))
