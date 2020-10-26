(defpackage :cl-gserver.actor-system-test
  (:use :cl :fiveam :cl-gserver.actor-system-api :cl-gserver.actor-system
        :cl-gserver.dispatcher)
  (:import-from #:act
                #:actor
                #:make-actor)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.actor-system-test)

(def-suite actor-system-tests
  :description "Tests for the actor system"
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
  (let ((system (make-actor-system :shared-dispatcher-workers 4)))
    (unwind-protect
         (progn
           (is (not (null system)))
           (is (not (null (message-dispatcher system))))
           (is-true (typep (message-dispatcher system) 'shared-dispatcher))
           (is (= 4 (length (workers (message-dispatcher system))))))
      (shutdown system)
      (sleep 1))))

(test create-system--check-defaults
  "Checking defaults on the system"
  (let ((system (make-actor-system)))
    (unwind-protect
         (progn
           (is-true (typep (message-dispatcher system) 'shared-dispatcher))
           (is (= 4 (length (workers (message-dispatcher system))))))
      (shutdown system))))

(test create-actors--shared
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :disp-type :shared)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-dp))
      (is (not (null (act-cell:system actor))))
      (is (= 1 (length (ac:actors cut))))
      (is (eq actor (aref (ac:actors cut) 0))))))

(test create-actors--pinned
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :disp-type :pinned)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-bt))
      (is (not (null (act-cell:system actor))))
      (is (= 1 (length (ac:actors cut))))
      (is (eq actor (aref (ac:actors cut) 0))))))

(test creating-many-actors--and-collect-responses
  "Creating many actors should not pose a problem."
  (with-fixture test-system ()
    (let ((actors (loop for i from 1 to 10000
                        collect (ac:actor-of
                                 cut
                                 (lambda ()
                                   (make-actor
                                    (lambda (self msg state)
                                      (declare (ignore self))
                                      (cons (format nil "reply: ~a" msg) state))))))))
      (log:debug "Starting ask...")
      (is-true (every (lambda (x) (string= "reply: test" x))
                      (mapcar (lambda (actor)
                                (act:ask actor "test"))
                              actors)))
      (log:debug "Starting ask...done"))))

(defun run-tests ()
  (run! 'create-system)
  (run! 'create-actors--shared)
  (run! 'create-actors--pinned)
  (run! 'creating-many-actors--and-collect-responses))
