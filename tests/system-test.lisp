(defpackage :cl-gserver.system-test
  (:use :cl :fiveam
        :cl-gserver.system :cl-gserver.system-api
        :cl-gserver.actor :cl-gserver.actor-context)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.system-test)

(def-suite system-tests
  :description "Tests for the GServer system"
  :in cl-gserver.tests:test-suite)

(in-suite system-tests)

(def-fixture test-system ()
  (let ((cut (make-system)))
    (unwind-protect
         (&body)
      (shutdown cut)
      (sleep 1))))

(test create-system
  "Creates a system"
  (let ((system (make-system)))
    (unwind-protect
         (progn
           (is (not (null system)))
           (is (not (null (dispatcher system)))))
      (shutdown system)
      (sleep 1))))

(test create-actors
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (actor-of cut (lambda () (make-instance 'actor
                                                    :receive-fun (lambda ()))))))
      (print actor)
      (is (not (null actor)))
      (is (not (null (the-system actor))))
      (is (= 1 (length (get-actors cut))))
      (is (eq actor (car (get-actors cut)))))))

(defun run-tests ()
  (run! 'create-system)
  (run! 'create-actors))
