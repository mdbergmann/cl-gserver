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

(test setf-integer
  "Test atomic-setf for integer type."
  (let ((value (make-atomic-integer :value 0))
        (minus -1)
        (plus  1)
        (zero  0))
    (is (= minus (atomic-setf value minus)))
    (is (= minus (atomic-get value)))
    (is (= plus (atomic-setf value plus)))
    (is (= plus (atomic-get value)))
    (is (= zero (atomic-setf value zero)))
    (is (= zero (atomic-get value)))))

(test setf-object
  "Test atomic-setf other than integer type"
  (let* ((ref (make-atomic-reference :value '()))
         (kwd :kwd)
         (int 8)
         (lst '(1 2))
         (obj (make-array 1))
         (empty nil))
    (is (eq kwd (atomic-setf ref kwd)))
    (is (eq kwd (atomic-get ref)))
    (is (eq int (atomic-setf ref int)))
    (is (eq int (atomic-get ref)))
    (is (eq lst (atomic-setf ref lst)))
    (is (eq lst (atomic-get ref)))
    (is (eq obj (atomic-setf ref obj)))
    (is (eq obj (atomic-get ref)))
    (is (eq empty (atomic-setf ref empty)))
    (is (eq empty (atomic-get ref)))))
