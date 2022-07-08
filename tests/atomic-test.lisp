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
  (let* ((ref (make-atomic-reference :value '(0)))
         (fn1 #'append)
         (args (list 1 2))
         (fn2 (lambda (lst) (apply #'+ lst))))
    (is (equal '(0 1 2) (atomic-swap ref fn1 args))) ; test with rest args
    (is (equal '(0 1 2) (atomic-get ref)))
    (is (= 3 (atomic-swap ref fn2))) ; test without rest args
    (is (= 3 (atomic-get ref)))))

(test make-atomic-integer
  "Tests making an atomic integer"
  (is (make-atomic-integer :value 5)))

(test swap-integer
  "Swap an intreger value."
  (let* ((value (make-atomic-integer :value 5))
         (fn1 #'+)
         (fn2 #'1+))
    (is (= 8 (atomic-swap value fn1 1 2)))
    (is (= 8 (atomic-get value)))
    (is (= 9 (atomic-swap value fn2)))
    (is (= 9 (atomic-get value)))))
