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

(defclass stash-actor (act:actor stashing) ())

(test create-actor-with-stash
  (with-fixture test-context ()
    (is (not (null (ac:actor-of system
                                :type 'stash-actor
                                :receive (lambda (self msg state)
                                           (declare (ignore self msg state)))))))))

(test stash-actor-can-stash-messages
  (with-fixture test-context ()
    (let ((cut (ac:actor-of system
                            :type 'stash-actor
                            :receive (lambda (self msg state)
                                       (declare (ignore state))
                                       (stash:stash self msg)))))
      (act:tell cut :to-be-stashed-msg)
      (is-true (utils:assert-c 0.5
                 (= (length (stash::stashed-messages cut)) 1))))))

(test stash-actor-can-unstash-messages
  (with-fixture test-context ()
    (let* ((do-stash-message t)
           (handled-unstash nil)
           (cut (ac:actor-of system
                             :type 'stash-actor
                             :receive
                             (lambda (self msg state)
                               (when do-stash-message
                                 (stash:stash self msg))
                               (unless do-stash-message
                                 (case msg
                                   (:unstash
                                    (progn
                                      (stash:unstash-all self)
                                      (cons :unstashed state)))
                                   (:to-be-stashed-msg
                                    (progn
                                      (setf handled-unstash t)
                                      (cons :no-reply state)))))))))
      (act:tell cut :to-be-stashed-msg)
      (utils:assert-c 0.5 (has-stashed-messages cut))
      (setf do-stash-message nil)
      (is (eq :unstashed (act:ask-s cut :unstash)))
      (is-true handled-unstash)
      (is-false (has-stashed-messages cut)))))


(run! 'stash-tests)
