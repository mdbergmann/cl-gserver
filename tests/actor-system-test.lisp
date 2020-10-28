(defpackage :cl-gserver.actor-system-test
  (:use :cl :fiveam :cl-gserver.actor-system)
  (:import-from #:act
                #:actor
                #:make-actor)
  (:import-from #:utils
                #:assert-cond)
  (:import-from #:dispatcher
                #:workers
                #:shared-dispatcher)
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
      (ac:shutdown cut)
      (sleep 0.5))))

(test create-system
  "Creates a system"
  (let ((system (make-actor-system :shared-dispatcher-workers 4)))
    (is (not (null system)))
    (is (not (null (sys::system-actor-context system))))
    (is (typep (sys::system-actor-context system) 'ac:actor-context))
    (is (not (null (sys::user-actor-context system))))
    (is (typep (sys::user-actor-context system) 'ac:actor-context))
    (ac:shutdown system)
    (sleep 0.5)))

(test shutdown-system
  "Shutting down should stop all actors whether pinned or shared."
  (let ((system (make-actor-system)))
    (sys::%actor-of system (lambda () (make-actor (lambda ()))) :pinned :context-key :user)
    (sys::%actor-of system (lambda () (make-actor (lambda ()))) :shared :context-key :user)
    (sys::%actor-of system (lambda () (make-actor (lambda ()))) :pinned :context-key :system)
    (sys::%actor-of system (lambda () (make-actor (lambda ()))) :shared :context-key :system)

    (ac:shutdown system)
    (is-true (assert-cond (lambda ()
                            (= 0 (length (ac:find-actors
                                          system
                                          (lambda (actor) (act-cell:running-p actor)))))) 2))))

(test create-system--check-defaults
  "Checking defaults on the system"
  (let ((system (make-actor-system)))
    (let ((dispatchers (dispatchers system)))
      (is-true (typep (getf dispatchers :shared) 'shared-dispatcher))
      (is (= 4 (length (workers (getf dispatchers :shared))))))
    (ac:shutdown system)))

(test actor-of--shared-user
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :dispatch-type :shared)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-dp))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      (is (= 1 (length (ac:all-actors (sys::user-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (sys::user-actor-context cut))))))))

(test actor-of--shared-system
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (sys::%actor-of cut (lambda () (make-actor (lambda ()))) :shared :context-key :system)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-dp))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      (is (= 1 (length (ac:all-actors (sys::system-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (sys::system-actor-context cut))))))))

(test actor-of--pinned-user
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :dispatch-type :pinned)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-bt))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      (is (= 1 (length (ac:all-actors (sys::user-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (sys::user-actor-context cut))))))))

(test actor-of--pinned-system
  "Creates actors in the system."
  (with-fixture test-system ()
    (let ((actor (sys::%actor-of cut (lambda () (make-actor (lambda ()))) :pinned :context-key :system)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box-bt))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      (is (= 1 (length (ac:all-actors (sys::system-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (sys::system-actor-context cut))))))))

(test find-actors--in-system
  "Test finding actors in system."
  (with-fixture test-system ()
    (let ((act1 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo"))))
          (act2 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo2"))))
          (act3 (sys::%actor-of cut (lambda () (make-actor (lambda ()) :name "foo")) :shared :context-key :system))
          (act4 (sys::%actor-of cut (lambda () (make-actor (lambda ()) :name "foo2")) :shared :context-key :system)))
      (is (eq act1 (car (ac:find-actors cut (lambda (x) (string= "foo" (act-cell:name x)))))))
      (is (eq act2 (car (ac:find-actors cut (lambda (x) (string= "foo2" (act-cell:name x)))))))
      (is (eq act3 (car (sys::%find-actors cut (lambda (x) (string= "foo" (act-cell:name x))) :context-key :system))))
      (is (eq act4 (car (sys::%find-actors cut (lambda (x) (string= "foo2" (act-cell:name x))) :context-key :system))))
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
  (run! 'actor-of--shared-user)
  (run! 'actor-of--shared-system)
  (run! 'actor-of--pinned-user)
  (run! 'actor-of--pinned-system)
  (run! 'find-actors--in-system)
  (run! 'creating-many-actors--and-collect-responses))
