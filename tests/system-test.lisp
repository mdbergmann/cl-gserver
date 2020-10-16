(defpackage :cl-gserver.system-test
  (:use :cl :fiveam :cl-gserver.system)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.system-test)

(def-suite system-tests
  :description "Tests for the GServer system"
  :in cl-gserver.tests:test-suite)

(in-suite system-tests)

(test create-system
  "Creates a system"
  (let ((system (make-system)))
    (is (not (null system)))
    (is (not (null (dispatcher system))))
    (terminate system)))

(defun runtests ()
  (run! 'create-system))
