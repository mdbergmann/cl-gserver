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

(def-fixture actor-fixture (receive state with-context)
  (let ((cut (make-actor receive
                         :state state
                         :name "test-actor")))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt))
    (when with-context
      (setf (act:context cut)
            (ac:make-actor-context
             (asys:make-actor-system '(:dispatchers (:shared (:workers 1)))))))
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

(defclass custom-actor (actor) ())
(test make-actor--custom-type
  "Tests making an actor instance that is not the default 'actor type."
  (is (typep (make-actor "foo" :type 'custom-actor) 'custom-actor)))

(test make-actor--with-init-and-destroy
  "Tests the `init' and `destroy' hooks."
  (let* ((init-called nil)
         (destroy-called nil)
         (actor (make-actor "foo-receive"
                            :init (lambda (self)
                                    (assert (not (null self)))
                                    ;; is called from `ac:actor-of' when fully initialized.
                                    (setf init-called t))
                            :destroy (lambda (self)
                                       (assert (not (null self)))
                                       (setf destroy-called t)))))
    (is-false init-called)
    (act-cell:stop actor)
    (is-true destroy-called)
  ))

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
    (is (eq t (assert-cond (lambda () (= 1 (ask-s cut "get"))) 1)))
    (is (= 5 (ask-s cut "bar")))
    (is (= 5 (ask-s cut "get")))
    (ask-s cut :stop)))

(test actor-of--from-existing-actor-context
  "Tests that a new 'child' actor can be created from an actor context."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg state)))
                               0
                               t)
    (let ((child-actor (actor-of ((act:context cut))
                         :receive (lambda (self msg state)
                                    (declare (ignore self msg state))))))
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
    (let ((watcher (actor-of ((act:context cut))
                     :receive (lambda ()))))
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
           (watcher (actor-of ((ac:system (act:context cut)))
                      :receive (lambda (self msg state)
                                 (declare (ignore self))
                                 (case (car msg)
                                   (:stopped (progn
                                               (assert (eq cut (cdr msg)))
                                               (setf stopped-msg-received t)
                                               (cons msg state))))))))
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
    (let ((child-actor (actor-of ((act:context cut))
                         :receive (lambda (self msg state)
                                    (declare (ignore self msg state))))))
      (act-cell:stop cut)
      (is (assert-cond (lambda ()
                         (notany
                          #'act-cell:running-p
                          (list cut child-actor))) 1)))))

(test stop-actor--notifies-actor-context-as-watcher
  "Tests that when actor stops and an actor-context is a watcher, that it is notified about the stop."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg state)))
                               0
                               t)
    (let ((child-actor (actor-of ((act:context cut))
                         :receive (lambda (self msg state)
                                    (declare (ignore self msg state))))))
      (with-mocks ()
        (answer (ac:notify context actor event)
          (progn
            (assert (eq context (act:context cut)))
            (assert (eq actor child-actor))
            (assert (eq :stopped event))))
        (act-cell:stop cut)
        (is (= 1 (length (invocations 'ac:notify))))))))

(test single-actor--handle-ask
  "Tests the async ask-s function."

  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self))
                                 (sleep 0.2)
                                 (cond
                                   ((eq :add (car msg))
                                    (cons (+ (second msg) (third msg)) state))))
                               0
                               nil)
    (let ((future (ask cut '(:add 0 5))))
      (is (eq :not-ready (get-result future)))
      (is (eq t (assert-cond (lambda () (complete-p future)) 1)))
      (is (= 5 (get-result future))))))


(test single-actor--handle-ask-2
  "Test handle a composable asynchronous call. on-completed before completion."

  (with-fixture actor-fixture ((lambda (self msg state)
                                        (declare (ignore self))
                                        (sleep 0.5)
                                        (cond
                                          ((eq :add (car msg))
                                           (cons (+ (second msg) (third msg)) state))))
                               0
                               nil)
    (let ((future (ask cut '(:add 0 5)))
          (on-completed-result nil))
      (is (eq :not-ready (get-result future)))
      (on-completed future (lambda (result)
                             (setf on-completed-result result)))
      (is (eq t (assert-cond (lambda () (complete-p future)) 1)))
      (is (= 5 (get-result future))))))

(test ask-s--shared--timeout
  "Tests for ask-s timeout."
  (with-fixture actor-fixture ((lambda ())
                               0
                               t)
    (let* ((actor (actor-of ((act:context cut))
                    :receive (lambda (self msg state)
                               (declare (ignore self msg))
                               (sleep 2)
                               (cons :my-result state))))
           (result (ask-s actor "foo" :time-out 0.5)))
      ;; we're expecting a timeout error here. But BT on CCL raises an 'interrupted' error.
      (is (eq :handler-error (car result)))
      (is (typep (cdr result) 'utils:ask-timeout))
      (is (eq :stopped (ask-s actor :stop))))))

(test ask-s--shared--timeout-in-dispatcher
  "Tests for ask-s timeout."
  (with-fixture actor-fixture ((lambda ())
                               0
                               t)
    (with-mocks ()
      (answer (disp:dispatch-async _ _)
        (progn
          (format t "Dispatch called...~%")
          (sleep 2)))
      (let* ((actor (actor-of ((act:context cut))
                      :receive (lambda (self msg state)
                                 (declare (ignore self msg state)))))
             (result (ask-s actor "foo" :time-out 0.5)))
        ;; we're expecting a timeout error here. But BT on CCL raises an 'interrupted' error.
        (is (eq :handler-error (car result)))
        (is (typep (cdr result) 'utils:ask-timeout))
        (is (= 1 (length (invocations 'disp:dispatch-async))))))))

(test ask--shared--timeout
  "Tests for ask timeout."
  (with-fixture actor-fixture ((lambda ())
                               0
                               t)
    (let* ((actor (actor-of ((act:context cut))
                    :receive (lambda (self msg state)
                               (declare (ignore self msg))
                               (sleep 2)
                               (cons :my-result state))))
           (future (ask actor "foo" :time-out 0.5)))
      (utils:wait-cond (lambda () (complete-p future)))
      (is (eq :handler-error (car (get-result future))))
      (is (typep (cdr (get-result future)) 'utils:ask-timeout)))))

(test ask-s--pinned--timeout
  "Tests for ask-s timeout."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg))
                                 (sleep 2)
                                 (cons :my-result state))
                               0
                               nil)
    (let ((result (ask-s cut "foo" :time-out 0.5)))
      (is (eq :handler-error (car result)))
      (is (typep (cdr result) 'utils:ask-timeout))
      (is (eq :stopped (ask-s cut :stop))))))

(test ask--pinned--timeout
  "Tests for ask timeout."
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg))
                                 (sleep 2)
                                 (cons :my-result state))
                               0
                               nil)
    (let ((future (ask cut "foo" :time-out 0.5)))
      (utils:wait-cond (lambda () (complete-p future)))
      (is (eq :handler-error (car (get-result future))))
      (is (typep (cdr (get-result future)) 'utils:ask-timeout)))))

(test allow--no-reply--response
  "Tests to allow `:no-reply' for `tell', `ask-s' and `ask'"
  (with-fixture actor-fixture ((lambda (self msg state)
                                 (declare (ignore self msg))
                                 (if act-cell:*sender*
                                     (tell act-cell:*sender* :manual-reply))
                                 (cons :no-reply state))
                               0
                               t)
    (is-true (tell cut :foo))
    (is (eq :no-reply (ask-s cut :foo)))
    (let ((fut (ask cut :foo)))
      (assert-cond (lambda () (complete-p fut)) 1.0)
      (is (eq :manual-reply (get-result fut))))
  ))

(test become-and-unbecome-a-different-behavior
  "Test switching behaviors"
  (let ((receive (lambda (self msg state)
                   (declare (ignore self msg))
                   (cons :receive state)))
        (beh1 (lambda (self msg state)
                (declare (ignore self msg))
                (cons :behavior1 state)))
        (beh2 (lambda (self msg state)
                (declare (ignore self msg))
                (cons :behavior2 state))))  
    (with-fixture actor-fixture (receive 0 t)
      (is (eq :receive (ask-s cut :some)))
      (become cut beh1)
      (is (eq :behavior1 (ask-s cut :some)))
      (become cut beh2)
      (is (eq :behavior2 (ask-s cut :some)))
      (unbecome cut)
      (is (eq :receive (ask-s cut :some))))))

(test actor-of-macro
  "Tests the convenience actor-of macro"
  (let ((sys (asys:make-actor-system)))
    (unwind-protect
         (let* ((receive-fun (lambda (self msg state)
                               (declare (ignore self))
                               (when (string= "Foo" msg)
                                 (cons "Bar" state))))
                (actor (actor-of (sys) :receive receive-fun))
                (custom-actor (actor-of (sys) :receive receive-fun :type 'custom-actor)))
           (is (string= "Bar" (ask-s actor "Foo")))
           (is (string= "Bar" (ask-s custom-actor "Foo")))
           (is (typep custom-actor 'custom-actor)))
      (ac:shutdown sys))))

(test actor-of-macro--figure-context
  "Tests to conveniently specify three types as context: asys, ac and actor"
  (let ((sys (asys:make-actor-system)))
    (unwind-protect
         (let* ((receive-fun (lambda (self msg state)
                               (declare (ignore self))
                               (when (string= "Foo" msg)
                                 (cons "Bar" state))))
                (system-actor (actor-of (sys "foo1") :receive receive-fun))
                (ac-actor (actor-of ((act:context system-actor) "foo2") :receive receive-fun))
                (actor-actor (actor-of (ac-actor "foo3") :receive receive-fun)))
           (is (string= "Bar" (ask-s system-actor "Foo")))
           (is (string= "/user/foo1" (path system-actor)))
           (is (string= "Bar" (ask-s ac-actor "Foo")))
           (is (string= "/user/foo1/foo2" (path ac-actor)))
           (is (string= "Bar" (ask-s actor-actor "Foo")))
           (is (string= "/user/foo1/foo2/foo3" (path actor-actor))))
      (ac:shutdown sys))))

(test actor-of-macro--with-init-and-receive
  "Tests the macro by specifying the receive function and init function as plist parameters."
  (let ((sys (asys:make-actor-system)))
    (unwind-protect
         (let* ((init-called nil)
                (actor (actor-of (sys "foo")
                         :init (lambda (self)
                                 (declare (ignore self))
                                 (setf init-called t))
                         :receive (lambda (self msg state)
                                    (declare (ignore self))
                                    (when (string= "Foo" msg)
                                      (cons "Bar" state))))))
           (is (string= "Bar" (ask-s actor "Foo")))
           (is-true init-called))
      (ac:shutdown sys))))
