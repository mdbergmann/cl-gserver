(defpackage :cl-gserver.tests
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:nil
           #:test-suite))

(in-package :cl-gserver.tests)

(def-suite test-suite
  :description "All catching test suite.")

(in-suite test-suite)

(test foo
  "Trivial test"
  (is (= 1 1)))
