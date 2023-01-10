(defpackage :sento.miscutils-test
  (:use :cl :fiveam :sento.miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.miscutils-test)

(def-suite sento.miscutils-test
  :description "Suite for utils"
  :in sento.tests:test-suite)

(in-suite sento.miscutils-test)

(test print-condition-backtrace
  "Check printing condition backtrace"
  (is (> (length (collect-backtrace (make-condition 'error))) 0)))
