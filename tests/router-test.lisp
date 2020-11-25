(defpackage :cl-gserver.router-test
  (:use :cl :fiveam :cl-mock :cl-gserver.router)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.router-test)

(def-suite router-tests
  :description "Tests for router"
  :in cl-gserver.tests:test-suite)

(in-suite router-tests)

(test router--create
  "Creates a plain router"
  (is (not (null (make-router))))
  (is (typep (make-router) 'router)))

(defparameter *fake-context* "fake-context")

(test router--add-routee
  "Tests adding routees (actors)"
  (with-mocks ()
    (answer (ac:actor-of _ create-fun) (funcall create-fun))
    
    (let ((cut (make-router)))
      (dotimes (i 2)
        (add-routee cut (ac:actor-of *fake-context*
                                     (lambda ()
                                       (act:make-actor (lambda (self msg state)
                                                         (declare (ignore self))
                                                         (cons msg state)))))))
      (is (= 2 (length (routees cut)))))))
