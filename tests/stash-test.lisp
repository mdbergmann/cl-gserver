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
                                       (stash:stash self msg)
                                       (cons :no-reply state)))))
      (act:tell cut :to-be-stashed-msg)
      (is-true (utils:await-cond 0.5
                 (has-stashed-messages-p cut))))))

(test stash-actor-can-unstash-messages-with-preserving-sender
  (with-fixture test-context ()
    (let* ((do-stash-message t)
           (received-msg nil)
           (sender (ac:actor-of system
                                :receive
                                (lambda (self msg state)
                                  (setf received-msg msg)
                                  (cons nil state))))
           (cut (ac:actor-of system
                             :type 'stash-actor
                             :receive
                             (lambda (self msg state)
                               (if do-stash-message
                                   (progn 
                                     (stash:stash self msg)
                                     (cons :no-reply state))
                                   (case msg
                                     (:unstash
                                      (progn
                                        (stash:unstash-all self)
                                        (cons :unstashed state)))
                                     (:to-be-stashed-msg
                                      (progn
                                        (act:tell act-cell:*sender* :stashed-msg-reply)
                                        (cons :no-reply state)))))))))
      (act:tell cut :to-be-stashed-msg sender)
      (utils:await-cond 0.5 (has-stashed-messages-p cut))
      (setf do-stash-message nil)
      (is (eq :unstashed (act:ask-s cut :unstash)))
      (is-true (utils:await-cond 0.5
                 (eq received-msg :stashed-msg-reply))))))

(test unstash-order-is-as-stash-order
  "Checks that `unstash-all' unstashes in the same order as messages were stashed."
  (with-fixture test-context ()
    (let* ((do-stash-message t)
           (unstashed-recv '())
           (cut (ac:actor-of system
                             :type 'stash-actor
                             :receive
                             (lambda (self msg state)
                               (if do-stash-message
                                   (progn 
                                     (stash:stash self msg)
                                     (cons :no-reply state))
                                   (case msg
                                     (:unstash
                                      (progn
                                        (stash:unstash-all self)
                                        (cons :unstashed state)))
                                     (otherwise
                                      (progn
                                        (setf unstashed-recv (cons msg unstashed-recv))
                                        (cons :no-reply state))))))))
           (msgs '(msg-1 msg-2 msg-3 msg-4 msg-5)))
      (loop :for msg in msgs
            :do (act:tell cut msg))
      (utils:await-cond 0.5 (= (length (stash::stashed-messages cut)) 5))
      (setf do-stash-message nil)
      (act:ask-s cut :unstash)
      (is-true (utils:await-cond 0.5 (= (length unstashed-recv) 5)))
      (is (equalp msgs (reverse unstashed-recv))))))
