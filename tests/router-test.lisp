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

(defparameter *fake-context* "fake-context")

(defun make-fake-actor ()
  (ac:actor-of *fake-context*
               (lambda ()
                 (act:make-actor (lambda (self msg state)
                                   (declare (ignore self))
                                   (cons msg state))))))

(test router--create
  "Creates a plain router"
  (is (not (null (make-router))))
  (is (typep (make-router) 'router))
  (is (functionp (strategy (make-router)))))

(test router--add-routee
  "Tests adding routees (actors)"
  (with-mocks ()
    (answer (ac:actor-of _ create-fun) (funcall create-fun))
    
    (let ((cut (make-router)))
      (dotimes (i 5)
        (add-routee cut (make-fake-actor)))
      (is (= 5 (length (routees cut)))))))

(test router--provide-routees-at-contructor
  "Provide routees when constructing."
  (with-mocks ()
    (answer (ac:actor-of _ create-fun) (funcall create-fun))
    
    (let ((cut (make-router :routees (list
                                      (make-fake-actor)
                                      (make-fake-actor)
                                      (make-fake-actor)))))
      (is (= 3 (length (routees cut)))))))

(test router--stop
  "Stopping router stops routees."
  (with-mocks ()
    (answer (ac:actor-of _ create-fun) (funcall create-fun))
    (answer (act-cell:stop _) t)
    
    (let ((cut (make-router)))
      (dotimes (i 2)
        (add-routee cut (make-fake-actor)))
      (is (= 2 (length (routees cut))))
      (is (equalp '(t t) (stop cut)))
    (is (= 2 (length (invocations 'act-cell:stop)))))))

(test router--tell
  "Tests 'tell' on the router which forwards to an actor chosen after the strategy."
  (with-mocks ()
    (answer (ac:actor-of _ create-fun) (funcall create-fun))
    
    (let ((cut (make-router :routees (list
                                      (make-fake-actor)
                                      (make-fake-actor)))))
      (is (equalp (loop :repeat 5
                        :collect :no-message-handling)
                  (loop :repeat 5
                        :collect (tell cut "Foo")))))))
  

(defun run-tests ()
  (run! 'router--create)
  (run! 'router--add-routee)
  (run! 'router--provide-routees-at-contructor)
  (run! 'router--stop)
  (run! 'router--tell))
