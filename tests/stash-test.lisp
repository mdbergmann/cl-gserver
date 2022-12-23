(defpackage :sento.stash-test
  (:use :cl :fiveam :sento.stash)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.stash-test)

(def-suite stash-tests
  :description "Tests for stash mixin"
  :in sento.tests:test-suite)

(in-suite stash-tests)

(def-fixture test-context ()
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 1))))))
    (unwind-protect
         (&body)
      (ac:shutdown system :wait t))))

(defclass stash-actor (act:actor stash) ())

(test create-actor-with-stash
  (with-fixture test-context ()
    (is (not (null (ac:actor-of system
                                :type 'stash-actor
                                :receive (lambda (self msg state)
                                           (descare (ignore self msg state)))))))))


(run! 'create-actor-with-stash)
