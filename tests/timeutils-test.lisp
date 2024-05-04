(defpackage :sento.timeutils-test
  (:use :cl :fiveam :sento.timeutils)
  (:import-from #:miscutils
                #:await-cond)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.timeutils-test)

(def-suite sento.timeutils-test
  :description "Suite for utils"
  :in sento.tests:test-suite)

(in-suite sento.timeutils-test)

(test timer--create
  "Test create timer"
  (is (typep (make-timer 0.1 (lambda ())) 'bt2:thread)))

(test timer--run
  "Tests the timer run."
  (let ((run-ok nil))
    (make-timer 0.1 (lambda () (setf run-ok t)))
    (is-true (await-cond 1 run-ok))))
