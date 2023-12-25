(defpackage :sento.wheel-timer-test
  (:use :cl :fiveam :sento.wheel-timer)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.wheel-timer-test)

(def-suite wheel-timer-tests
  :description "Tests for wheel timer"
  :in sento.tests:test-suite)

(in-suite wheel-timer-tests)

(test make-wheel-timer
  "Tests making a wheel timer with config"
  (let ((cut (make-wheel-timer :resolution 100 :max-size 100)))
    (unwind-protect
         (progn
           (is (not (null cut))))
      (shutdown-wheel-timer cut))))

(test schedule
  "Tests executing a scheduled timer function."
  (let ((cut (make-wheel-timer :resolution 100 :max-size 100))
        (callback))
    (unwind-protect
         (progn
           (schedule cut 0.2 (lambda () (setf callback t)))
           (is-true (miscutils:assert-cond (lambda () (eq t callback)) 0.25)))
      (shutdown-wheel-timer cut))))

(test schedule-recurring
  "Tests executing recurring timer functions."
  (let ((cut (make-wheel-timer))
        (callbacks 0))
    (unwind-protect
         (progn
           (schedule-recurring cut 0.1 'foo (lambda () (incf callbacks)))
           (is-true (miscutils:assert-cond (lambda () (>= callbacks 4)) 0.5 0.1)))
      (shutdown-wheel-timer cut))))

(test cancel-recurring-timer
  "Tests canceling a recurring timer function."
  (let ((cut (make-wheel-timer)))
    (unwind-protect
         (progn
           (schedule-recurring cut 0.1 'foo (lambda ()))
           (is-true (gethash 'foo (wt::timer-hash cut)))
           (cancel-for-sig cut 'foo)
           (is-false (gethash 'foo (wt::timer-hash cut))))
      (shutdown-wheel-timer cut))))
