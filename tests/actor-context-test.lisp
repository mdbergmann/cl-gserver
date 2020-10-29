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

(defvar *test-actor-system* (asys:make-actor-system :shared-dispatcher-workers 0))

(test create-with-default-constructor
  "Test if the default constructor creates a context."
  (is (not (null (make-actor-context *test-actor-system*)))))

(test create-context--check-aggregated-components
  "Tests creating a context."
  (let ((cut (make-actor-context *test-actor-system*)))
    (is (not (null cut)))
    (is (not (null (system cut))))))

(test create-actor--actor-of--shared
  "Tests creating a new actor in the context with shared dispatcher"
  (let* ((cut (make-actor-context *test-actor-system*))
         (actor (actor-of cut (lambda () (make-actor (lambda ()))))))
    (is (not (null actor)))
    (is (= 1 (length (all-actors cut))))
    (is (typep (act-cell:msgbox actor) 'mesgb:message-box/dp))
    (is (not (null (act:context actor))))
    (is (not (eq cut (act:context actor))))
    (is (not (null (ac:system (act:context actor)))))))

(test create-actor--actor-of--pinned
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

(test create-actor--dont-add-when-null-creator
  "Tests creating a new actor in the context."
  (let* ((cut (make-actor-context *test-actor-system*)))
    (actor-of cut (lambda () nil))
    (is (= 0 (length (all-actors cut))))))

(test find-actors-test
  "Test for finding actors"
  (let ((context (make-actor-context *test-actor-system*)))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
    (actor-of context (lambda () (make-actor (lambda ()) :name "foo2")))
    (is (= 2 (length
              (find-actors context
                           (lambda (actor) (str:starts-with-p "foo" (act-cell:name actor)))))))))

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

(defun run-tests ()
  (run! 'create-with-default-constructor)
  (run! 'create-context--check-aggregated-components)
  (run! 'create-actor--actor-of--shared)
  (run! 'create-actor--actor-of--pinned)
  (run! 'create-actor--dont-add-when-null-creator)
  (run! 'find-actors-test)
  (run! 'retrieve-all-actors)
  (run! 'shutdown-actor-context)
  )
