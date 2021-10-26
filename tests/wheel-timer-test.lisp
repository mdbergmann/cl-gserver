(defpackage :cl-gserver.wheel-timer-test
  (:use :cl :fiveam :cl-gserver.wheel-timer)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.wheel-timer-test)

(def-suite wheel-timer-tests
  :description "Tests for wheel timer"
  :in cl-gserver.tests:test-suite)

(in-suite wheel-timer-tests)

(test make-wheel-timer
  "Tests making a wheel timer with config"
  (let ((cut (make-wheel-timer '(:resolution 100 :max-size 100))))
    (is (not (null cut)))
    (shutdown-wheel-timer cut)))
