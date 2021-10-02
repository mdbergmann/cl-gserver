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

(test swap
  "Swap a reference."
  (let* ((ref (make-atomic-reference :value '()))
         (old (atomic-reference-value ref)))
    (is-true (atomic-reference-cas ref old '(1 2)))
    (is (equal '(1 2) (atomic-reference-value ref)))))
