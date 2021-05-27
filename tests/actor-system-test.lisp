(defpackage :cl-gserver.actor-system-test
  (:use :cl :fiveam :cl-mock :cl-gserver.actor-system)
  (:import-from #:act
                #:actor
                #:make-actor)
  (:import-from #:utils
                #:assert-cond)
  (:import-from #:disp
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
      (ac:shutdown cut))))

(test create-system--default-config
  "Creates an actor-system by applying the default config."
  (let ((system (make-actor-system)))
    (is (not (null system)))
    (is (not (null (asys::internal-actor-context system))))
    (is (string= "/internal" (ac:id (asys::internal-actor-context system))))
    (is (typep (asys::internal-actor-context system) 'ac:actor-context))
    (is (not (null (asys::user-actor-context system))))
    (is (string= "/user" (ac:id (asys::user-actor-context system))))
    (is (typep (asys::user-actor-context system) 'ac:actor-context))
    (is (= 4 (length (disp:workers (getf (asys::dispatchers system) :shared)))))
    (ac:shutdown system)
    ;;(sleep 0.2)
    ))

(test create-system--custom-config
  "Create an actor-system by passing a custom config."
  (let ((system (make-actor-system '(:dispatchers (:shared (:workers 2))))))
    (is (= 2 (length (disp:workers (getf (asys::dispatchers system) :shared)))))
    (ac:shutdown system)))

(test create-system--additional-dispatcher
  "Creates an actor system with an additional custom dispatcher."
  (let ((system (make-actor-system '(:dispatchers (:foo (:workers 3))))))
    (is (= 4 (length (disp:workers (getf (asys::dispatchers system) :shared)))))
    (is (= 3 (length (disp:workers (getf (asys::dispatchers system) :foo)))))
    (ac:shutdown system)))

(test create-system--check-defaults
  "Checking defaults on the system"
  (let ((system (make-actor-system)))
    (let ((dispatchers (dispatchers system)))
      (is-true (typep (getf dispatchers :shared) 'shared-dispatcher))
      (is (= 4 (length (workers (getf dispatchers :shared))))))
    (ac:shutdown system)))

(test shutdown-system
  "Shutting down should stop all actors whether pinned or shared.
We use internal API here only for this test, do not use this otherwise."
  (let ((system (make-actor-system)))
    (asys::%actor-of system (lambda () (make-actor (lambda ()))) :pinned :context-key :user)
    (asys::%actor-of system (lambda () (make-actor (lambda ()))) :shared :context-key :user)
    (asys::%actor-of system (lambda () (make-actor (lambda ()))) :pinned :context-key :internal)
    (asys::%actor-of system (lambda () (make-actor (lambda ()))) :shared :context-key :internal)

    (ac:shutdown system)
    (is-true (assert-cond (lambda ()
                            (= 0 (length (ac:find-actors
                                          system
                                          (lambda (actor) (act-cell:running-p actor)))))) 2))))

(test actor-of--verify-proper-root-path
  "Tests whether actors and contexts are created with proper paths."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo")) :dispatcher-id :shared)))
      (is (string= "/user/foo" (act:path actor)))
      (is (string= "/user/foo" (ac:id (act:context actor)))))))

(test actor-of--shared--user
  "Creates actors in the system in user context with shared dispatcher."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :dispatcher-id :shared)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box/dp))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      (is (= 1 (length (ac:all-actors (asys::user-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (asys::user-actor-context cut))))))))

(test actor-of--shared--internal
  "Creates actors in the system in internal context with shared dispatcher."
  (with-fixture test-system ()
    (let ((actor (asys::%actor-of cut (lambda () (make-actor (lambda ()))) :shared :context-key :internal)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box/dp))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      ;; 1 here, + 1 eventstream + 4 dispatch-workers
      (is (= 6 (length (ac:all-actors (asys::internal-actor-context cut)))))
      ;; first is eventstream actor
      (is (member actor (ac:all-actors (asys::internal-actor-context cut)) :test #'eq)))))

(test actor-of--pinned--user
  "Creates actors in the system in user context with pinned dispatcher."
  (with-fixture test-system ()
    (let ((actor (ac:actor-of cut (lambda () (make-actor (lambda ()))) :dispatcher-id :pinned)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box/bt))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      (is (= 1 (length (ac:all-actors (asys::user-actor-context cut)))))
      (is (eq actor (first (ac:all-actors (asys::user-actor-context cut))))))))

(test actor-of--pinned--internal
  "Creates actors in the system in internal context with pinned dispatcher."
  (with-fixture test-system ()
    (let ((actor (asys::%actor-of cut (lambda () (make-actor (lambda ()))) :pinned :context-key :internal)))
      (is (not (null actor)))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box/bt))
      (is (not (null (act:context actor))))
      (is (eq (ac:system (act:context actor)) cut))
      ;; 1 here, + 1 eventstream + 4 dispatch-workers
      (is (= 6 (length (ac:all-actors (asys::internal-actor-context cut)))))
      ;; first is eventstream actor
      (is (member actor (ac:all-actors (asys::internal-actor-context cut)) :test #'eq)))))

(test find-actors--in-system
  "Test finding actors in system."
  (with-fixture test-system ()
    (let ((act1 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo"))))
          (act2 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo2"))))
          (act3 (asys::%actor-of cut (lambda () (make-actor (lambda ()) :name "foo")) :shared :context-key :internal))
          (act4 (asys::%actor-of cut (lambda () (make-actor (lambda ()) :name "foo2")) :shared :context-key :internal)))
      (is (eq act1 (first (ac:find-actors cut (lambda (x) (string= "foo" (act-cell:name x)))))))
      (is (eq act2 (first (ac:find-actors cut (lambda (x) (string= "foo2" (act-cell:name x)))))))
      (is (eq act3 (first (asys::%find-actors cut (lambda (x) (string= "foo" (act-cell:name x))) :context-key :internal))))
      (is (eq act4 (first (asys::%find-actors cut (lambda (x) (string= "foo2" (act-cell:name x))) :context-key :internal))))
      (is (eq nil (ac:find-actors cut (lambda (x) (declare (ignore x))))))
      (is (= 2 (length (ac:find-actors cut #'identity)))))))

(test find-actor-by-name--in-system
  "Tests finding an actor by name."
  (with-fixture test-system ()
    (let ((act1 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo"))))
          (act2 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo2")))))
      (is (eq act1 (ac:find-actor-by-name cut "foo")))
      (is (eq act2 (ac:find-actor-by-name cut "foo2"))))))

(test all-actors--in-system-user-context
  "Retrieves all actors in user actor context of system."
  (with-fixture test-system ()
    (let ((act1 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo"))))
          (act2 (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo2")))))
      (is (= 2 (length (ac:all-actors cut))))
      (is (some (lambda (x) (eq act1 x)) (ac:all-actors cut)))
      (is (some (lambda (x) (eq act2 x)) (ac:all-actors cut))))))

(test stop-actor--in-system
  "Tests stopping an actor. This pretty much does the same as the method in actor-context."
  (with-fixture test-system ()
    (with-mocks ()
      (let ((act (ac:actor-of cut (lambda () (make-actor (lambda ()) :name "foo"))))
            (call-to-stop-done nil))
        (answer (act-cell:stop actor-to-stop)
          (progn
            (assert (eq actor-to-stop act))
            (setf call-to-stop-done t)
            nil))
        (ac:stop cut act)
        (is-true (assert-cond (lambda () call-to-stop-done) 1))
        (is (= 1 (length (invocations 'act-cell:stop))))))))

(test creating-some-actors--and-collect-responses
  "Creating many actors should not pose a problem."
  (with-fixture test-system ()
    (let ((actors (loop :repeat 100
                        collect (act:actor-of (cut)
                                  :receive (lambda (self msg state)
                                             (declare (ignore self))
                                             (cons (format nil "reply: ~a" msg) state)))))
          (ask-result nil))
      (time (setf ask-result
                  (every (lambda (x) (string= "reply: test" x))
                         (mapcar (lambda (actor)
                                   (act:ask-s actor "test"))
                                 actors))))
      (is-true ask-result))))

(test ev-subscribe-publish-receive--all-messages
  "Subscribe to eventstream, publish and receive. IntegTest."
  (with-fixture test-system ()
    (let* ((ev-received)
           (ev-listener (act:actor-of (cut)
                          :receive (lambda (self msg state)
                                     (declare (ignore self state))
                                     (setf ev-received msg)
                                     (cons nil nil)))))
      (ev:subscribe (eventstream cut) ev-listener)
      (ev:publish (eventstream cut) "Foo")
      (is (assert-cond (lambda () (equal ev-received "Foo")) 1))
      (ev:unsubscribe (eventstream cut) ev-listener))))
