(defpackage :cl-gserver.system-test
  (:use :cl :fiveam
        :cl-gserver.system-api :cl-gserver.system
        :cl-gserver.actor :cl-gserver.system-actor :cl-gserver.actor-context)
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
           (is (not (null (get-dispatcher system)))))
      (shutdown system)
      (sleep 1))))

(test create-actors
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (actor-of cut (lambda () (make-instance 'actor
                                                    :receive-fun (lambda ()))))))
      (is (not (null actor)))
      (is (not (null (get-system actor))))
      (is (= 1 (length (get-actors cut))))
      (is (eq actor (car (get-actors cut)))))))

(test creating-many-actors
  "Creating many actors should not pose a problem."
  (with-fixture test-system ()
    (let ((actors (loop for i from 1 to 10000
                        collect (actor-of
                                 cut
                                 (lambda ()
                                   (make-instance
                                    'actor
                                    :receive-fun
                                    (lambda (self msg state)
                                      (declare (ignore self))
                                      (cons (format nil "reply: ~a" msg) state))))))))
      (log:debug "Starting ask...")
      (is-true (every (lambda (x) (string= "reply: test" x))
                      (mapcar (lambda (actor)
                                (ask actor "test"))
                              actors)))
      (log:debug "Starting ask...done"))))

(defun run-tests ()
  (run! 'create-system)
  (run! 'create-actors)
  (run! 'creating-many-actors))
