(defpackage :cl-gserver.system-test
  (:use :cl :fiveam :cl-gserver.system :cl-gserver.system-api :cl-gserver.actor)
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
      (shutdown cut))))

(test create-system
  "Creates a system"
  (let ((system (make-system)))
    (is (not (null system)))
    (is (not (null (dispatcher system))))
    (shutdown system)))

(test shutdown-system
  "Tests shutting down a system and stopping all dispatcher mailboxes."
  (flet ((len-message-threads () (length
                                  (remove-if-not (lambda (x)
                                                   (str:starts-with-p "message-thread-mb" x))
                                                 (mapcar #'bt:thread-name (bt:all-threads))))))
    (let* ((len-message-threads-before (len-message-threads))
           (system (make-system)))
      (is (= (+ len-message-threads-before 4) (len-message-threads)))
      (shutdown system)
      (is (eq t (cl-gserver.gserver-test:assert-cond
                 (lambda ()
                   (= len-message-threads-before (len-message-threads)))
                 2))))))

(test create-actors
  "Creates actors in the system."

  (with-fixture test-system ()
    (let ((actor (actor-of cut (lambda () (make-actor)))))
      (is (not (null actor)))
      (is (not (null (gs::system actor))))
      (is (= 1 (length (actors cut))))
      (is (eq actor (car (actors cut)))))))

(defun run-tests ()
  (run! 'create-system)
  (run! 'create-actors))
