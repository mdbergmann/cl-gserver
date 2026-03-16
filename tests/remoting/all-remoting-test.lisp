(defpackage :sento.remoting.tests
  (:use :cl :fiveam)
  (:export #:remoting-test-suite))

(in-package :sento.remoting.tests)

(def-suite remoting-test-suite
  :description "All remoting tests.")

(in-suite remoting-test-suite)
