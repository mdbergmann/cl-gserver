(defpackage :cl-gserver.router-test
  (:use :cl :fiveam :cl-gserver.router)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.router-test)

(def-suite router-tests
  :description "Tests for router"
  :in cl-gserver.tests:test-suite)

(in-suite router-tests)

(test create-router
  "Creates a plain router"
  (is (not (null (make-router))))
  (is (typep (make-router) 'router))
  )
