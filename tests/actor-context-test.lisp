(defpackage :cl-gserver.actor-context-test
  (:use :cl :fiveam :cl-gserver.actor-context :act)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.actor-context-test)

(def-suite actor-context-tests
  :description "Tests for actor context"
  :in cl-gserver.tests:test-suite)

(in-suite actor-context-tests)

(defclass test-context (actor-context) ())
(defmethod actor-of ((self test-context) create-fun &key disp-type)
  (declare (ignore disp-type))
  (let ((created (funcall create-fun)))
    (when created
      (add-actor self created))))

(test create-with-default-constructor
  "Test if the defauilt constructor creates a context."
  (is (not (null (make-actor-context)))))

(test create-context
  "Tests creating a context."
  (is (not (null (make-instance 'test-context)))))

(test create-actor--actor-of
  "Tests creating a new actor in the context."
  (let* ((cut (make-instance 'test-context))
         (actor (actor-of cut (lambda () (make-instance 'actor
                                                   :receive-fun (lambda ()))))))
    (is (not (null actor)))
    (is (= 1 (length (actors cut))))))

(test create-actor--dont-add-when-null-creator
  "Tests creating a new actor in the context."
  (let* ((cut (make-instance 'test-context)))
    (actor-of cut (lambda () nil))
    (is (= 0 (length (actors cut))))))


(defun run-tests ()
  (run! 'create-with-default-constructor)
  (run! 'create-context)
  (run! 'create-actor--actor-of)
  (run! 'create-actor--dont-add-when-null-creator))
