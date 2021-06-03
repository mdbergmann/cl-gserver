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

(def-fixture test-system ()
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 0))))))
    (unwind-protect
         (&body)
      (ac:shutdown system))))

(test create--with-default-constructor
  "Test if the default constructor creates a context."
  (with-fixture test-system ()
    (is (not (null (make-actor-context system))))
    (is (null (id (make-actor-context system))))
    (is (string= "foo" (id (make-actor-context system "foo"))))))

(test create--check-aggregated-components
  "Tests creating a context."
  (with-fixture test-system ()
    (let ((cut (make-actor-context system)))
      (is (not (null cut)))
      (is (not (null (system cut)))))))

(test actor-of--creates-actor-path-and-context-id
  "Tests that when actors are created using `actor-of' that they get an expanded path from the context `id'."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system "/foo"))
           (actor (actor-of cut (lambda () (make-actor (lambda ()) :name "bar")))))
      (print actor)
      (is (string= "/foo/bar" (act:path actor)))
      (is (string= "/foo/bar" (id (act:context actor)))))))

(test actor-of--calls-initialized-hook
  "Check that 'initialized' hook got called during actor-of."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system "/foo"))
           (init-called nil)
           (actor (actor-of cut (lambda () (make-actor (lambda ())
                                                  :name "bar"
                                                  :init (lambda (self)
                                                          (assert (not (null (act:context self))))
                                                          (setf init-called t)))))))
      (declare (ignore actor))
      (is-true init-called))))

(test actor-of--shared
  "Tests creating a new actor in the context with shared dispatcher"
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut (lambda () (make-actor (lambda ()))))))
      (is (not (null actor)))
      (is (= 1 (length (all-actors cut))))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box/dp))
      (is (not (null (act:context actor))))
      (is (not (eq cut (act:context actor))))
      (is (not (null (ac:system (act:context actor))))))))

(test actor-of--pinned
  "Tests creating a new actor in the context with pinned dispatcher"
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut (lambda () (make-actor (lambda ()))) :dispatcher-id :pinned)))
      (is (not (null actor)))
      (is (= 1 (length (all-actors cut))))
      (is (typep (act-cell:msgbox actor) 'mesgb:message-box/bt))
      (is (not (null (act:context actor))))
      (is (not (eq cut (act:context actor))))
      (is (not (null (ac:system (act:context actor)))))
      ;; stop the :pinned actor
      (act-cell:stop actor))))

(test actor-of--custom-dispatcher
  "Tests creating an actor with a custom shared dispatcher."
  (let ((system))
    (unwind-protect
         (progn
           (setf system (asys:make-actor-system '(:dispatchers (:foo (:workers 0)))))
           (let* ((cut (make-actor-context system))
                  (actor (actor-of cut (lambda () (make-actor (lambda ()))) :dispatcher-id :foo)))
             (is (not (null actor)))
             (is (typep (act-cell:msgbox actor) 'mesgb:message-box/dp))
             (is (eq :foo (slot-value (mesgb::dispatcher (act-cell:msgbox actor)) 'disp::identifier)))
             ))
      (ac:shutdown system))))

(test actor-of--err-unknown-dispatcher
  "Tests creating a new actor on an unknown dispatcher."
  (with-fixture test-system ()
    (let ((cut (make-actor-context system)))
      (handler-case
          (progn
            (actor-of cut (lambda () (make-actor (lambda ()))) :dispatcher-id :unknown)
            (fail()))
        (error (c)
          (format t "cond: ~a~%" c)
          (assert (string= "No such dispatcher identifier 'UNKNOWN' exists!"
                           (format nil "~a" c)))
          (is-true t))))))

(test actor-of--dont-add-when-null-creator
  "Tests creating a new actor in the context."
  (with-fixture test-system ()
    (let ((cut (make-actor-context system)))
      (actor-of cut (lambda () nil))
      (is (= 0 (length (all-actors cut)))))))

(test actor-of--adds-itself-as-watcher
  "Tests that when creating a new actor that the ac watches the actor."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut (lambda () (make-actor (lambda ()) :name "foo")))))
      (is (not (null (find-if
                      (lambda (e) (eq e cut))
                      (act:watchers actor))))))))

(test notify--actor-stopped--remove-from-actors-list
  "Tests that the actor is removed from the actors list when a notification of `:stopped' is received."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut (lambda () (make-actor (lambda ()) :name "foo")))))
      (is (= 1 (length (all-actors cut))))
      (notify cut actor :stopped)
      (is (= 0 (length (all-actors cut)))))))

(test find-actors-test
  "Test for finding actors"
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
      (actor-of context (lambda () (make-actor (lambda ()) :name "foo2")))
      (is (= 2 (length
                (find-actors context
                             (lambda (actor) (str:starts-with-p "foo" (act-cell:name actor))))))))))

(test stop-actor--by-context
  "Tests stopping an actor."
  (with-fixture test-system ()
    (with-mocks ()
      (let* ((cut (make-actor-context system))
             (actor (actor-of cut (lambda () (make-actor (lambda ())))))
             (cell-stop-called nil))
        (answer (act-cell:stop actor-to-stop)
          (progn 
            (assert (eq actor-to-stop actor))
            (setf cell-stop-called t)
            nil))
        (stop cut actor)
        (is-true cell-stop-called)
        (is (= 1 (length (invocations 'act-cell:stop))))))))

(test retrieve-all-actors
  "Retrieves all actors"
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
      (actor-of context (lambda () (make-actor (lambda ()) :name "foo2")))
      (is (= 2 (length (all-actors context)))))))

(test shutdown-actor-context
  "Shutdown actor context - stop all actors."
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (actor-of context (lambda () (make-actor (lambda ()) :name "foo")))
      (actor-of context (lambda () (make-actor (lambda ()) :name "foo2")))
      (shutdown context)
      (is (= 0 (length (find-actors context (lambda (a) (act-cell:running-p a)))))))))

(test enforce-unique-actor-names
  "Test that all actors in a context have a unique name."
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
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
          (is-true t))))))

(test remove-and-add-actors--find-actor-by-name
  "Tests removing and again adding actors !!! this tests private API"
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (ac::%add-actor context (make-instance 'act-cell:actor-cell :name :foo))
      (is (find-actor-by-name context :foo))
      (ac::%remove-actor context (make-instance 'act-cell:actor-cell :name :foo))
      (is (= 0 (length (all-actors context))))
      (ac::%add-actor context (make-instance 'act-cell:actor-cell :name :foo))
      (is (find-actor-by-name context :foo)))))
