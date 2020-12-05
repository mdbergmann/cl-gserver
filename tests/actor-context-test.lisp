(defpackage :cl-gserver.actor-context-test
  (:use :cl :fiveam :cl-mock :cl-gserver.actor-context)
  (:import-from #:act
                #:make-actor)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.actor-context-test)

(def-suite actor-context-tests
  :description "Tests for actor context"
  :in cl-gserver.tests:test-suite)

(in-suite actor-context-tests)

(defvar *test-actor-system* (asys:make-actor-system :shared-dispatcher-workers 0))

(test create--with-default-constructor
  "Test if the default constructor creates a context."
  (is (not (null (make-actor-context *test-actor-system*)))))

(test create--check-aggregated-components
  "Tests creating a context."
  (let ((cut (make-actor-context *test-actor-system*)))
    (is (not (null cut)))
    (is (not (null (system cut))))))

(test actor-of--shared
  "Tests creating a new actor in the context with shared dispatcher"
  (let* ((cut (make-actor-context *test-actor-system*))
         (actor (actor-of cut (lambda () (make-actor (lambda ()))))))
    (is (not (null actor)))
    (is (= 1 (length (all-actors cut))))
    (is (typep (act-cell:msgbox actor) 'mesgb:message-box/dp))
    (is (not (null (act:context actor))))
    (is (not (eq cut (act:context actor))))
    (is (not (null (ac:system (act:context actor)))))))

(test actor-of--pinned
  "Tests creating a new actor in the context with pinned dispatcher"
  (let* ((cut (make-actor-context *test-actor-system*))
         (actor (actor-of cut (lambda () (make-actor (lambda ()))) :dispatch-type :pinned)))
    (is (not (null actor)))
    (is (= 1 (length (all-actors cut))))
    (is (typep (act-cell:msgbox actor) 'mesgb:message-box/bt))
    (is (not (null (act:context actor))))
    (is (not (eq cut (act:context actor))))
    (is (not (null (ac:system (act:context actor)))))
    ;; stop the :pinned actor
    (act-cell:stop actor)))

(test actor-of--dont-add-when-null-creator
  "Tests creating a new actor in the context."
  (let ((cut (make-actor-context *test-actor-system*)))
    (actor-of cut (lambda () nil))
    (is (= 0 (length (all-actors cut))))))

(test actor-of--adds-itself-as-watcher
  "Tests that when creating a new actor that the ac watches the actor."
  (let* ((cut (make-actor-context *test-actor-system*))
         (actor (actor-of cut (lambda () (make-actor (lambda ()) :name "foo")))))
    (is (not (null (find-if
                    (lambda (e) (eq e cut))
                    (act:watchers actor)))))))

(test notify--actor-stopped--remove-from-actors-list
  "Tests that the actor is removed from the actors list when a notification of `:stopped' is received."
  (let* ((cut (make-actor-context *test-actor-system*))
         (actor (actor-of cut (lambda () (make-actor (lambda ()) :name "foo")))))
    (is (= 1 (length (all-actors cut))))
    (notify cut actor :stopped)
    (is (= 0 (length (all-actors cut))))))

(test find-actors-test
  "Test for finding actors"
  (let ((context (make-actor-context *test-actor-system*)))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo2")))
    (is (= 2 (length
              (find-actors context
                           (lambda (actor) (str:starts-with-p "foo" (act-cell:name actor)))))))))

(test stop-actor--by-context
  "Tests stopping an actor."
  (with-mocks ()
    (let* ((cut (make-actor-context *test-actor-system*))
           (actor (actor-of cut (lambda () (make-actor (lambda ())))))
           (cell-stop-called nil))
      (answer (act-cell:stop actor-to-stop)
        (progn 
          (assert (eq actor-to-stop actor))
          (setf cell-stop-called t)
          nil))
      (stop cut actor)
      (is-true cell-stop-called)
      (is (= 1 (length (invocations 'act-cell:stop)))))))

(test retrieve-all-actors
  "Retrieves all actors"
  (let ((context (make-actor-context *test-actor-system*)))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo2")))
    (is (= 2 (length (all-actors context))))))

(test shutdown-actor-context
  "Shutdown actor context - stop all actors."
  (let ((context (make-actor-context *test-actor-system*)))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo2")))
    (shutdown context)
    (is (= 0 (length (find-actors context (lambda (a) (act-cell:running-p a))))))))

(test enforce-unique-actor-names
  "Test that all actors in a context have a unique name."
  (let ((context (make-actor-context *test-actor-system*)))
    (handler-case
        (progn
          (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
          (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
          (format t "Foo~%")
          (fail))
      (error (c)
        (format t "cond: ~a~%" c)
        (assert (string= "Actor with name 'foo' already exists!"
                         (format nil "~a" c)))
        (is-true t)))))

(test remove-and-add-actors--find-by-name
  "Tests removing and again adding actors !!! this tests private API"
  (let ((context (make-actor-context *test-actor-system*)))
    (ac::add-actor context (make-instance 'act-cell:actor-cell :name :foo))
    (is (find-by-name context :foo))
    (ac::remove-actor context (make-instance 'act-cell:actor-cell :name :foo))
    (is (= 0 (length (all-actors context))))
    (ac::add-actor context (make-instance 'act-cell:actor-cell :name :foo))
    (is (find-by-name context :foo))
  ))

(defun run-tests ()
  (run! 'create--with-default-constructor)
  (run! 'create--check-aggregated-components)
  (run! 'actor-of--shared)
  (run! 'actor-of--pinned)
  (run! 'actor-of--dont-add-when-null-creator)
  (run! 'actor-of--adds-itself-as-watcher)
  (run! 'notify--actor-stopped--remove-from-actors-list)
  (run! 'find-actors-test)
  (run! 'stop-actor--by-context)
  (run! 'retrieve-all-actors)
  (run! 'shutdown-actor-context)
  (run! 'enforce-unique-actor-names)
  (run! 'remove-and-add-actors)
  )
