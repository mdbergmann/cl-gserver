(defpackage cl-test-proj/tests/main
  (:use :cl
        :cl-test-proj
        :fiveam))
(in-package :cl-test-proj/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-test-proj)' in your Lisp.

;; rove example
(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

;; fiveam example
(test trivial
  "Trivial test"
  (is (= 1 1)))
