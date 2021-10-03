(defpackage :cl-gserver.atomic-test
  (:use :cl :fiveam :cl-gserver.atomic)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.atomic-test)

(def-suite atomic-tests
  :description "Tests for atomic."
  :in cl-gserver.tests:test-suite)

(in-suite atomic-tests)

(test make-atomic-reference
  "Tests making an atomic reference"
  (is (make-atomic-reference :value '())))

(test swap-reference
  "Swap a reference."
  (let* ((ref (make-atomic-reference :value '()))
         (old (atomic-get ref)))
    (is-true (atomic-cas ref old '(1 2)))
    (is (equal '(1 2) (atomic-get ref)))))

(test make-atomic-integer
  "Tests making an atomic integer"
  (is (make-atomic-integer :value 5)))

(test swap-integer
  "Swap an intreger value."
  (let* ((value (make-atomic-integer :value 5))
         (old (atomic-get value)))
    (is-true (atomic-cas value old 2))))
