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

(def-fixture actor-fixture (receive-fun state)
  (defclass test-actor (actor) ())
  (defmethod receive ((actor test-actor) message current-state)
    (funcall receive-fun actor message current-state))

  (let ((cut (make-instance 'test-actor :state state)))  
    (&body)
    (send cut :stop)))

(def-fixture simple-actor-fixture (receive-fun state)
  (let ((cut (make-actor "Foo"
                         :state state
                         :receive-fun (lambda (self message state) (funcall receive-fun self message state)))))
    (&body)
    (send cut :stop)))


(test custom-actor
  "Test a subclass of actor."

  (with-fixture actor-fixture ((lambda (actor message current-state)
                                 (declare (ignore actor))
                                 (cond
                                   ((string= message "foo") (cons 1 1))
                                   ((string= message "bar") (cons 5 5))
                                   ((string= message "get") (cons current-state current-state))))
                               0)
    (is (not (null cut)))
    (is (eq t (send cut "foo")))
    (sleep 0.1)
    (is (equal 1 (ask cut "get")))
    (is (equal 5 (ask cut "bar")))
    (is (equal 5 (ask cut "get")))))


(test simple-actor
  "The simplified actor."

  (with-fixture simple-actor-fixture ((lambda (self message current-state)
                                        (declare (ignore self))
                                        (cond
                                          ((string= message "foo") (cons 1 1))
                                          ((string= message "bar") (cons 5 5))
                                          ((string= message "get") (cons current-state current-state))))
                                      0)
      (is (not (null cut)))
      (is (eq t (send cut "foo")))
      (sleep 0.1)
      (is (equal 1 (ask cut "get")))
      (is (equal 5 (ask cut "bar")))
      (is (equal 5 (ask cut "get")))))

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
