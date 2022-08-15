(defpackage :sento.utils-test
  (:use :cl :fiveam :sento.utils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.utils-test)

(def-suite sento.utils-test
  :description "Suite for utils"
  :in sento.tests:test-suite)

(in-suite sento.utils-test)

(test timer--create
  "Test create timer"
  (is (typep (make-timer 0.1 (lambda ())) 'bt:thread)))

(test timer--run
  "Tests the timer run."
  (let ((run-ok nil))
    (make-timer 0.1 (lambda () (setf run-ok t)))
    (is-true (assert-cond (lambda () (eq t run-ok)) 1))))

(test print-condition-backtrace
  "Check printing condition backtrace"
  (is (> (length (collect-backtrace (make-condition 'error))) 0)))
