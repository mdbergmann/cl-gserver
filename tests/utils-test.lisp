(defpackage :cl-gserver.utils-test
  (:use :cl :fiveam :cl-gserver.utils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.utils-test)

(def-suite cl-gserver.utils-test
  :description "Suite for utils"
  :in cl-gserver.tests:test-suite)

(in-suite cl-gserver.utils-test)

;; your test code here

(test mkstr
  "Trivial test"
  (is (string= (mkstr 'foo " hello " 1234) "FOO hello 1234")))

(run! 'mkstr)
