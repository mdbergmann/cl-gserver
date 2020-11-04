(defpackage :cl-gserver.actor-test
  (:use :cl :fiveam :cl-mock :cl-gserver.actor :cl-gserver.future)
  (:import-from #:utils
                #:assert-cond)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.actor-test)

(def-suite actor-tests
  :description "actor tests"
  :in cl-gserver.tests:test-suite)

(in-suite actor-tests)

(def-fixture actor-fixture (receive-fun state with-context)
  (defclass test-actor (actor) ())
  (let ((cut (make-actor receive-fun
                         :state state)))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt))
    (when with-context
      (setf (act:context cut)
            (ac:make-actor-context
             (asys:make-actor-system :shared-dispatcher-workers 1))))
    (unwind-protect
         (&body)
      (progn
        (when with-context
          (ac:shutdown (ac:system (act:context cut))))
        (tell cut :stop)))))

(test get-actor-name-and-state
  "Tests that the actor has the proper name and state after creating it."
  (let ((cut (make-actor (lambda (self msg state)
                           (declare (ignore self msg state)) nil)
                         :state '(1)
                         :name "Foo")))
    (is (string= "Foo" (act-cell:name cut)))
    (is (equalp '(1) (act-cell:state cut)))))

(test make-actor--has-no-msgbox-and-actor-context
  "Test constructor. actor should not have msgbox and attached actor-context."

  (let ((actor (make-actor (lambda (self msg state)
                             (declare (ignore self msg state))
                             nil))))
    (is (null (act-cell:msgbox actor)))
    (is (null (context actor)))))

(test make-actor--with-msgbox
  "Test contructor and attach a msgbox manually."

  (let ((cut (make-actor (lambda (self msg state)
                             (declare (ignore self))
                             (cond
                               ((string= msg "foo") (cons 1 1))
                               ((string= msg "bar") (cons 5 5))
                               ((string= msg "get") (cons state state)))))))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt))
    (is (not (null cut)))
    (is (eq t (tell cut "foo")))
    (is (eq t (assert-cond (lambda () (= 1 (ask cut "get"))) 1)))
    (is (= 5 (ask cut "bar")))
    (is (= 5 (ask cut "get")))
    (ask cut :stop)))

(test actor-of--from-existing-actor-context
  "Tests that a new 'child' actor can be created from an actor context."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg state)))
                               0
                               t)
    (let ((child-actor (ac:actor-of (act:context cut)
                                    (lambda () (make-actor (lambda (self msg state)
                                                        (declare (ignore self msg state))))))))
      (is (not (null child-actor)))
      (is (not (eq (act:context child-actor) (act:context cut))))
      (is (eq (ac:system (act:context child-actor)) (ac:system (act:context cut))))
      (is (eq child-actor (first (ac:all-actors (act:context cut))))))))

(test watch-and-unwatch--new-actor
  "Tests the 'watching' and 'unwatching' of a new actor which adds to 'watchers' on the target actor."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg state)))
                               0
                               t)
    (let ((watcher (ac:actor-of
                    (act:context cut)
                    (lambda () (make-actor (lambda ()))))))
      (watch cut watcher)
      (is (= 1 (length (watchers cut))))
      (unwatch cut watcher)
      (is (= 0 (length (watchers cut)))))))

(test watch--notify-about-stopped
  "Tests the notification of the `:stopped' lifecycle event of the actor."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg state)))
                               0
                               t)
    ;; we need an actor as watcher that is not the 'child' of the to be watched actor.
    (let* ((stopped-msg-received nil)
           (watcher (ac:actor-of
                     (ac:system (act:context cut))
                     (lambda () (make-actor (lambda (self msg state)
                                         (declare (ignore self))
                                         (case (car msg)
                                           (:stopped (progn
                                                       (assert (eq cut (cdr msg)))
                                                       (setf stopped-msg-received t)
                                                       (cons msg state))))))))))
      (watch cut watcher)
      (is (= 1 (length (watchers cut))))
      (ac:stop (act:context cut) cut)
      (is-true (assert-cond (lambda () stopped-msg-received) 1)))))

(test stop-actor--stopping-parent-stops-also-child
  "Tests that stopping a parent actor also the children are stopped."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg state)))
                               0
                               t)
    (let ((child-actor (ac:actor-of
                        (act:context cut)
                        (lambda () (make-actor (lambda (self msg state)
                                            (declare (ignore self msg state))))))))
      (act-cell:stop cut)
      (is (assert-cond (lambda ()
                         (notany
                          #'act-cell:running-p
                          (list cut child-actor))) 1)))))

(test single-actor--handle-async-ask
  "Tests the async ask function."

  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self))
                                 (sleep 0.2)
                                 (cond
                                   ((eq :add (car msg))
                                    (cons (+ (second msg) (third msg)) state))))
                               0
                               nil)
    (let ((future (async-ask cut '(:add 0 5))))
      (is (eq :not-ready (get-result future)))
      (is (eq t (assert-cond (lambda () (complete-p future)) 1)))
      (is (= 5 (get-result future))))))


(test single-actor--handle-async-ask-2
  "Test handle a composable asynchronous call. on-completed before completion."

  (with-fixture actor-fixture ((lambda (self msg state)
                                        (declare (ignore self))
                                        (sleep 0.5)
                                        (cond
                                          ((eq :add (car msg))
                                           (cons (+ (second msg) (third msg)) state))))
                               0
                               nil)
    (let ((future (async-ask cut '(:add 0 5)))
          (on-completed-result nil))
      (is (eq :not-ready (get-result future)))
      (on-completed future (lambda (result)
                             (setf on-completed-result result)))
      (is (eq t (assert-cond (lambda () (complete-p future)) 1)))
      (is (= 5 (get-result future))))))

(test ask--shared--timeout
  "Tests for ask timeout."
  (with-fixture actor-fixture ((lambda ())
                               0
                               t)
    (let* ((actor (ac:actor-of (ac:system (act:context cut))
                               (lambda ()
                                 (make-actor
                                  (lambda (self msg state)
                                    (declare (ignore self msg))
                                    (sleep 2)
                                    (cons :my-result state))))))
           (result (ask actor "foo" :time-out 0.5)))
      ;; we're expecting a timeout error here. But BT on CCL raises an 'interrupted' error.
      (is (eq :handler-error (car result)))
      (is (typep (cdr result) 'utils:ask-timeout))
      (is (eq :stopped (ask actor :stop))))))

(test ask--shared--timeout-in-dispatcher
  "Tests for ask timeout."
  (with-fixture actor-fixture ((lambda ())
                               0
                               t)
    (with-mocks ()
      (answer (disp:dispatch _ _)
        (progn
          (format t "Dispatch called...~%")
          (sleep 2)))
      (let* ((actor (ac:actor-of (ac:system (act:context cut))
                                 (lambda ()
                                   (make-actor
                                    (lambda (self msg state)
                                      (declare (ignore self msg state)))))))
             (result (ask actor "foo" :time-out 0.5)))
        ;; we're expecting a timeout error here. But BT on CCL raises an 'interrupted' error.
        (is (eq :handler-error (car result)))
        (is (typep (cdr result) 'utils:ask-timeout))
        (is (= 1 (length (invocations 'disp:dispatch))))))))

(test async-ask--shared--timeout
  "Tests for async-ask timeout."
  (with-fixture actor-fixture ((lambda ())
                               0
                               t)
    (let* ((actor (ac:actor-of (ac:system (act:context cut))
                               (lambda ()
                                 (make-actor
                                  (lambda (self msg state)
                                    (declare (ignore self msg))
                                    (sleep 2)
                                    (cons :my-result state))))))
           (future (async-ask actor "foo" :time-out 0.5)))
      (utils:wait-cond (lambda () (complete-p future)))
      (is (eq :handler-error (car (get-result future))))
      (is (typep (cdr (get-result future)) 'utils:ask-timeout)))))

(test ask--pinned--timeout
  "Tests for ask timeout."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg))
                                 (sleep 2)
                                 (cons :my-result state))
                               0
                               nil)
    (let ((result (ask cut "foo" :time-out 0.5)))
      (is (eq :handler-error (car result)))
      (is (typep (cdr result) 'utils:ask-timeout))
      (is (eq :stopped (ask cut :stop))))))

(test async-ask--pinned--timeout
  "Tests for async-ask timeout."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg))
                                 (sleep 2)
                                 (cons :my-result state))
                               0
                               nil)
    (let ((future (async-ask cut "foo" :time-out 0.5)))
      (utils:wait-cond (lambda () (complete-p future)))
      (is (eq :handler-error (car (get-result future))))
      (is (typep (cdr (get-result future)) 'utils:ask-timeout)))))

;; (test with-actor-macro
;;   "Test the with-actor macro."

;;   (with-actor
;;       (receive
;;        (cond
;;          ((string= msg "foo") (cons 1 1))
;;          ((string= msg "bar") (cons 5 5))
;;          ((string= msg "get") (cons state state))))
      
;;       (is (eq t (tell self "foo")))
;;       (sleep 0.1)
;;       (is (equal 1 (ask self "get")))
;;       (is (equal 5 (ask self "bar")))
;;       (is (equal 5 (ask self "get")))))

(defun run-tests ()
  (run! 'make-actor--has-no-msgbox-and-actor-context)
  (run! 'make-actor--with-msgbox)
  (run! 'actor-of--from-existing-actor-context)
  (run! 'watch-and-unwatch--new-actor)
  (run! 'watch--notify-about-stopped)
  (run! 'stop-actor--stopping-parent-stops-also-child)
  (run! 'single-actor--handle-async-ask)
  (run! 'single-actor--handle-async-ask-2)
  (run! 'ask--shared--timeout)
  (run! 'ask--shared--timeout-in-dispatcher)
  (run! 'async-ask--shared--timeout)
  (run! 'ask--pinned--timeout)
  (run! 'async-ask--pinned--timeout)
  ;;(run! 'with-actor-macro)
  )
