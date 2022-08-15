(defpackage :sento.actor-context-test
  (:use :cl :fiveam :cl-mock :sento.actor-context)
  (:import-from #:act
                #:make-actor)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.actor-context-test)

(def-suite actor-context-tests
  :description "Tests for actor context"
  :in sento.tests:test-suite)

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
           (actor (actor-of cut :name "bar" :receive (lambda ()))))
      (print actor)
      (is (string= "/foo/bar" (act:path actor)))
      (is (string= "/foo/bar" (id (act:context actor)))))))

(test actor-of--calls-initialized-hook
  "Check that 'initialized' hook got called during actor-of."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system "/foo"))
           (init-called nil)
           (actor (actor-of cut :name "bar"
                    :receive (lambda ())
                    :init (lambda (self)
                            (assert (not (null (act:context self))))
                            (setf init-called t)))))
      (declare (ignore actor))
      (is-true init-called))))

(test actor-of--shared
  "Tests creating a new actor in the context with shared dispatcher"
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut :receive (lambda ()))))
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
           (actor (actor-of cut :receive (lambda ()) :dispatcher :pinned)))
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
                  (actor (actor-of cut :receive (lambda ()) :dispatcher :foo)))
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
            (actor-of cut :receive (lambda ()) :dispatcher :unknown)
            (fail()))
        (error (c)
          (format t "cond: ~a~%" c)
          (assert (string= "No such dispatcher identifier 'UNKNOWN' exists!"
                           (format nil "~a" c)))
          (is-true t))))))

(test actor-of--dont-add-when-receive-is-not-function
  "Tests creating a new actor in the context that shouldn't be added to context."
  (with-fixture test-system ()
    (let ((cut (make-actor-context system)))
      (ignore-errors
       (actor-of cut :receive nil))
      (is (= 0 (length (all-actors cut)))))))

(test actor-of--adds-itself-as-watcher
  "Tests that when creating a new actor that the ac watches the actor."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut :name "foo" :receive (lambda ()))))
      (is (not (null (find-if
                      (lambda (e) (eq e cut))
                      (act:watchers actor))))))))

(test notify--actor-stopped--remove-from-actors-list
  "Tests that the actor is removed from the actors list when a notification of `:stopped' is received."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut :name "foo" :receive (lambda ()))))
      (is (= 1 (length (all-actors cut))))
      (notify cut actor :stopped)
      (is (= 0 (length (all-actors cut)))))))

(test find-actors--in-same-context-by-name
  "Test for finding actors"
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (actor-of context :name "foo" :receive (lambda ()))
      (actor-of context :name "foo2" :receive (lambda ()))
      (is (= 1 (length (find-actors context "foo" :test #'string=))))
      (is (= 1 (length (find-actors context "foo2" :test #'string=)))))))

(test find-actors--in-same-context--name-starts-with
  "Test for finding actors"
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (actor-of context :name "foo" :receive (lambda ()))
      (actor-of context :name "foo2" :receive (lambda ()))
      (is (= 2 (length (find-actors context "foo" :test #'str:starts-with-p)))))))

(test find-actors--in-child-context--by-name
  "Test for finding actors"
  (with-fixture test-system ()
    (let* ((context (make-actor-context system))
           (act1 (actor-of context :name "foo" :receive (lambda ()))))
      (actor-of (act:context act1) :name "foo2" :receive (lambda ()))
      (is (= 1 (length (find-actors context "foo/foo2" :test #'string=))))
      (is (= 1 (length (find-actors (act:context act1) "foo2")))))))

(test find-actors--in-child-context--error-on-path-component
  "Test for finding actors"
  (with-fixture test-system ()
    (let* ((context (make-actor-context system))
           (act1 (actor-of context :name "foo" :receive (lambda ()))))
      (actor-of (act:context act1) :name "foo2" :receive (lambda ()))
      (is (typep
           (handler-case
               (find-actors context "err/foo2")
             (error (c) c))
           'error)))))

(test find-actors--empty-path-argument
  "Test for finding actors"
  (with-fixture test-system ()
    (let* ((context (make-actor-context system))
           (act1 (actor-of context :name "foo" :receive (lambda ()))))
      (actor-of (act:context act1) :name "foo2" :receive (lambda ()))
      (is (null (find-actors context ""))))))

(test find-actors--from-root--delegate-to-system
  "Test for finding actors"
  (with-fixture test-system ()
    (let* ((context system)
           (act1 (actor-of context :name "foo" :receive (lambda ()))))
      (print context)
      (print act1)
      (actor-of (act:context act1) :name "foo2" :receive (lambda ()))
      (actor-of (act:context act1) :name "foo3" :receive (lambda ()))
      (is (= 1 (length (find-actors (make-actor-context system) "/user/foo/foo2"))))
      (is (= 2 (length (find-actors (make-actor-context system) "/user/foo/foo" :test #'str:starts-with-p)))))))

(test stop-actor--by-context
  "Tests stopping an actor."
  (with-fixture test-system ()
    (let* ((cut (make-actor-context system))
           (actor (actor-of cut :receive (lambda ()))))
      (stop cut actor)
      (is (eq :stopped (act:ask-s actor :foo))))))

(test retrieve-all-actors
  "Retrieves all actors"
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (actor-of context :name "foo" :receive (lambda ()))
      (actor-of context :name "foo2" :receive (lambda ()))
      (is (= 2 (length (all-actors context)))))))

(test shutdown-actor-context
  "Shutdown actor context - stop all actors."
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (actor-of context :name "foo" :receive (lambda ()))
      (actor-of context :name "foo2" :receive (lambda ()))
      (shutdown context)
      (is (= 0 (length (all-actors context)))))))

(test enforce-unique-actor-names
  "Test that all actors in a context have a unique name."
  (with-fixture test-system ()
    (let ((context (make-actor-context system)))
      (handler-case
          (progn
            (actor-of context :name "foo" :receive (lambda ()))
            (actor-of context :name "foo" :receive (lambda ()))
            (format t "Foo~%")
            (fail))
        (error (c)
          (format t "cond: ~a~%" c)
          (assert (string= "Actor with name 'foo' already exists!"
                           (format nil "~a" c)))
          (is-true t))))))
