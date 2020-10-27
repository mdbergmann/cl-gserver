(defpackage :cl-gserver.actor-system-test
  (:use :cl :fiveam :cl-gserver.actor-system-api :cl-gserver.actor-system
        :cl-gserver.dispatcher)
  (:import-from #:act
                #:actor
                #:make-actor)
  (:import-from #:utils
                #:assert-cond)
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
      (sleep 0.5))))

(test create-system
  "Creates a system"
  (let ((system (make-actor-system :shared-dispatcher-workers 4)))
    (unwind-protect
         (progn
           (is (not (null system)))
           (is (not (null (system::system-actor-context system))))
           (is (typep (system::system-actor-context system) 'ac:actor-context))
           (is (not (null (system::user-actor-context system))))
           (is (typep (system::user-actor-context system) 'ac:actor-context))
           (is (not (null (message-dispatcher system))))
           (is (typep (message-dispatcher system) 'shared-dispatcher))
           (is (= 4 (length (workers (message-dispatcher system))))))
      (shutdown system)
      (sleep 0.5))))

(test shutdown-system
  "Shutting down should stop all actors whether pinned or shared."
  (let ((system (make-actor-system)))
    (ac:actor-of system (lambda () (make-actor (lambda ()))) :disp-type :pinned)
    (ac:actor-of system (lambda () (make-actor (lambda ()))) :disp-type :shared)
    (system::%actor-of system (lambda () (make-actor (lambda ()))) :pinned :context :system)
    (system::%actor-of system (lambda () (make-actor (lambda ()))) :shared :context :system)

    (shutdown system)
    (is-true (assert-cond (lambda ()
                            (= 0 (length (ac:find-actors
                                          system
                                          (lambda (actor) (act-cell:running-p actor)))))) 2))))

(test create-system--check-defaults
  "Checking defaults on the system"
  (let ((system (make-actor-system)))
    (unwind-protect
         (progn
           (is-true (typep (message-dispatcher system) 'shared-dispatcher))
           (is (= 4 (length (workers (message-dispatcher system))))))
      (shutdown system))))

(test create-actors--shared-user
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :disp-type :shared)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-dp))
      (is (not (null (act-cell:system actor))))
      (is (= 1 (length (ac:all-actors (system::user-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (system::user-actor-context cut))))))))

(test create-actors--shared-system
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (system::%actor-of cut (lambda () (make-actor (lambda ()))) :shared :context :system)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-dp))
      (is (not (null (act-cell:system actor))))
      (is (= 1 (length (ac:all-actors (system::system-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (system::system-actor-context cut))))))))

(test create-actors--pinned-user
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :disp-type :pinned)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-bt))
      (is (not (null (act-cell:system actor))))
      (is (= 1 (length (ac:all-actors (system::user-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (system::user-actor-context cut))))))))

(test create-actors--pinned-system
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (system::%actor-of cut (lambda () (make-actor (lambda ()))) :pinned :context :system)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-bt))
      (is (not (null (act-cell:system actor))))
      (is (= 1 (length (ac:all-actors (system::system-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (system::system-actor-context cut))))))))

(test find-actors-in-system
  "Test finding actors in system."
  (with-fixture test-system ()
    (let ((act1 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo"))))
          (act2 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo2"))))
          (act3 (system::%actor-of cut (lambda () (make-actor (lambda ()) :name "foo")) :shared :context :system))
          (act4 (system::%actor-of cut (lambda () (make-actor (lambda ()) :name "foo2")) :shared :context :system)))
      (is (eq act1 (car (ac:find-actors cut (lambda (x) (string= "foo" (act-cell:name x)))))))
      (is (eq act2 (car (ac:find-actors cut (lambda (x) (string= "foo2" (act-cell:name x)))))))
      (is (eq act3 (car (system::%find-actors cut (lambda (x) (string= "foo" (act-cell:name x))) :context :system))))
      (is (eq act4 (car (system::%find-actors cut (lambda (x) (string= "foo2" (act-cell:name x))) :context :system))))
      (is (eq nil (ac:find-actors cut (lambda (x) (declare (ignore x))))))
      (is (= 2 (length (ac:find-actors cut #'identity)))))))

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
  (run! 'shutdown-system)
  (run! 'create-actors--shared-user)
  (run! 'create-actors--shared-system)
  (run! 'create-actors--pinned-user)
  (run! 'create-actors--pinned-system)
  (run! 'creating-many-actors--and-collect-responses))
