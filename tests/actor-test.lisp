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
      ((string= message "foo") (cons 1 1))
      ((string= message "bar") (cons 5 5))
      ((string= message "get") (cons current-state current-state))))
  
  (let ((cut (make-instance 'test-actor :state 0)))
    (is (not (null cut)))
    (is (eq t (send cut "foo")))
    (sleep 0.1)
    (is (equal 1 (ask cut "get")))
    (is (equal 5 (ask cut "bar")))
    (is (equal 5 (ask cut "get")))))


(test simple-actor
  "The simplified actor."

  (let ((cut (make-actor "Foo"
                         :state 0
                         :receive-fun
                         (lambda (self message current-state)
                           (cond
                             ((string= message "foo") (cons 1 1))
                             ((string= message "bar") (cons 5 5))
                             ((string= message "get") (cons current-state current-state)))))))
    (is (not (null cut)))
    (is (eq t (send cut "foo")))
    (sleep 0.1)
    (is (equal 1 (ask cut "get")))
    (is (equal 5 (ask cut "bar")))
    (is (equal 5 (ask cut "get")))    
  ))

;; (test with-actor-macro
;;   "Test the with-actor macro."

;;   (with-actor
;;       (receive
;;        (cond
;;          ((string= msg "foo") (cons 1 1))
;;          ((string= msg "bar") (cons 5 5))
;;          ((string= msg "get") (cons state state))))
      
;;       (is (eq t (send self "foo")))
;;       (sleep 0.1)
;;       (is (equal 1 (ask self "get")))
;;       (is (equal 5 (ask self "bar")))
;;       (is (equal 5 (ask self "get")))))

(run! 'custom-actor)
(run! 'simple-actor)
;;(run! 'with-actor-macro)
