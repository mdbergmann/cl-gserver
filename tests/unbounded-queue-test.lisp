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

(test unbounded-queue--try-popq-into
  "`try-popq-into' pops up to the vector's length elements into the vector and
returns how many, 0 on an empty queue, leaving the rest queued."
  (let ((cut (make-instance 'queue-unbounded))
        (batch (make-array 3 :initial-element nil)))
    (is (= 0 (try-popq-into cut batch)))
    (loop :for i :from 1 :to 5 :do (pushq cut i))
    (is (= 3 (try-popq-into cut batch)))
    (is (equalp #(1 2 3) batch))
    (is (= 2 (queued-count cut)))
    (is (= 2 (try-popq-into cut batch)))
    (is (equalp #(4 5 3) batch))
    (is (= 0 (queued-count cut)))
    (is-true (emptyq-p cut))
    (is (= 0 (try-popq-into cut batch)))))

(test unbounded-queue--popq--blocks-until-push
  "A `popq' on an empty queue blocks the calling thread and is woken by a
`pushq'. The waiter count the push consults is back at 0 afterwards."
  (let* ((cut (make-instance 'queue-unbounded))
         (result :none)
         (popper (bt2:make-thread (lambda () (setf result (popq cut))))))
    (is-true (miscutils:await-cond 0.5 (= 1 (slot-value cut 'sento.queue::waiters))))
    (is (eq :none result))
    (pushq cut :woken)
    (bt2:join-thread popper)
    (is (eq :woken result))
    (is (= 0 (slot-value cut 'sento.queue::waiters)))))

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
