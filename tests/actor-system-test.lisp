(defpackage :cl-gserver.actor-system-test
  (:use :cl :fiveam :cl-gserver.actor-system-api
   :cl-gserver.actor-system :cl-gserver.dispatcher
   :cl-gserver.actor
   :cl-gserver.actor-context)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.actor-system-test)

(def-suite actor-system-tests
  :description "Tests for the GServer system"
  :in cl-gserver.tests:test-suite)

(in-suite actor-system-tests)

(def-fixture test-system ()
  (let ((cut (make-actor-system)))
    (unwind-protect
         (&body)
      (shutdown cut)
      (sleep 1))))

(test create-system
  "Creates a system"
  (let ((system (make-actor-system :dispatcher-type 'shared-dispatcher
                                   :dispatcher-workers 4)))
    (unwind-protect
         (progn
           (is (not (null system)))
           (is (not (null (message-dispatcher system))))
           (is-true (typep (message-dispatcher system) 'shared-dispatcher))
           (is (= 4 (length (dispatcher-workers (message-dispatcher system))))))
      (shutdown system)
      (sleep 1))))

(test create-system--check-defaults
  "Checking defaults on the system"
  (let ((system (make-actor-system)))
    (unwind-protect
         (progn
           (is-true (typep (message-dispatcher system) 'shared-dispatcher))
           (is (= 4 (length (dispatcher-workers (message-dispatcher system))))))
      (shutdown system))))

(test create-actors
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (actor-of cut (lambda () (make-instance 'actor
                                                    :receive-fun (lambda ()))))))
      (is (not (null actor)))
      (is (not (null (act:system actor))))
      (is (= 1 (length (actors cut))))
      (is (eq actor (car (actors cut)))))))

;; (test creating-many-actors--and-collect-responses
;;   "Creating many actors should not pose a problem."
;;   (with-fixture test-system ()
;;     (let ((actors (loop for i from 1 to 10000
;;                         collect (actor-of
;;                                  cut
;;                                  (lambda ()
;;                                    (make-instance
;;                                     'actor
;;                                     :receive-fun
;;                                     (lambda (self msg state)
;;                                       (declare (ignore self))
;;                                       (cons (format nil "reply: ~a" msg) state))))))))
;;       (log:debug "Starting ask...")
;;       (is-true (every (lambda (x) (string= "reply: test" x))
;;                       (mapcar (lambda (actor)
;;                                 (ask actor "test"))
;;                               actors)))
;;       (log:debug "Starting ask...done"))))

;; (defun run-tests ()
;;   (run! 'create-system)
;;   (run! 'create-actors)
;;   (run! 'creating-many-actors--and-collect-responses))
