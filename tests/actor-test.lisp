(defpackage :cl-gserver.actor-test
  (:use :cl :fiveam :cl-gserver.actor)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.actor-test)

(def-suite actor-tests
  :description "actor tests"
  :in cl-gserver.tests:test-suite)

(in-suite actor-tests)

(test custom-actor
  "Test a subclass of actor."

  (defclass test-actor (actor) ())
  (defmethod receive ((self actor) message current-state)
    (cond
      ((eq message "foo") (cons 1 1))
      ((eq message "bar") (cons 5 5))
      ((eq message "get") (cons current-state current-state))))
  
  (let ((cut (make-instance 'test-actor :state 0)))
    (is (not (null cut)))
    (is (eq t (send cut "foo")))
    (sleep 0.1)
    (is (equal 1 (ask cut "get")))
    (is (equal 5 (ask cut "bar")))
    (is (equal 5 (ask cut "get")))))

(run! 'custom-actor)
