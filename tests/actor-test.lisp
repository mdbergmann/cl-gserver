(defpackage :sento.actor-test
  (:use :cl :fiveam :cl-mock :sento.actor :sento.future)
  (:import-from #:utils
                #:assert-cond
                #:await-cond)
  (:import-from #:ac
                #:actor-of)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :sento.actor-test)

(def-suite actor-tests
  :description "actor tests"
  :in sento.tests:test-suite)

(in-suite actor-tests)

(def-fixture actor-fixture (receive state with-context)
  (let ((cut (make-actor receive
                         :state state
                         :name "test-actor")))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt))
    (when with-context
      (setf (act:context cut)
            (ac:make-actor-context
             (asys:make-actor-system '(:dispatchers (:shared (:workers 2)))))))
    (unwind-protect
         (&body)
      (progn
        (when with-context
          (ac:shutdown (ac:system (act:context cut))))
        (tell cut :stop)))))

(test get-actor-name-and-state
  "Tests that the actor has the proper name and state after creating it."
  (let ((cut (make-actor (lambda (msg)
                           (declare (ignore msg)) nil)
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
  (let ((actor (make-actor (lambda (msg)
                             (declare (ignore msg))
                             nil))))
    (is (null (act-cell:msgbox actor)))
    (is (null (context actor)))))

(test make-actor--with-msgbox
  "Test contructor and attach a msgbox manually."
  (let ((cut (make-actor (lambda (msg)
                           (destructuring-bind (ret new-state)
                               (cond
                                 ((string= msg "foo") (list 1 1))
                                 ((string= msg "bar") (list 5 5))
                                 ((string= msg "get") (list *state* *state*)))
                             (setf *state* new-state)
                             ret)))))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt))
    (is (not (null cut)))
    (is-true (tell cut "foo"))
    (is-true (await-cond 0.5 (= 1 (ask-s cut "get"))))
    (is (= 5 (ask-s cut "bar")))
    (is (= 5 (ask-s cut "get")))
    (ask-s cut :stop)))

(test actor-of--from-existing-actor-context
  "Tests that a new 'child' actor can be created from an actor context."
  (with-fixture actor-fixture ((lambda (msg)
                                 (declare (ignore msg)))
                               0
                               t)
    (let ((child-actor (actor-of cut
                         :receive (lambda (msg)
                                    (declare (ignore msg))))))
      (is-false (null child-actor))
      (is-false (eq (act:context child-actor) (act:context cut)))
      (is (eq (ac:system (act:context child-actor)) (ac:system (act:context cut))))
      (is (eq child-actor (first (ac:all-actors (act:context cut))))))))

(test watch-and-unwatch--new-actor
  "Tests the 'watching' and 'unwatching' of a new actor which adds to 'watchers' on the target actor."
  (with-fixture actor-fixture ((lambda (msg)
                                 (declare (ignore msg)))
                               0
                               t)
    (let ((watcher (actor-of cut :receive (lambda (msg) (declare (ignore msg))))))
      (watch cut watcher)
      (is (= 1 (length (watchers cut))))
      (unwatch cut watcher)
      (is (= 0 (length (watchers cut)))))))

(test watch--notify-about-stopped
  "Tests the notification of the `:stopped' lifecycle event of the actor."
  (with-fixture actor-fixture ((lambda (msg)
                                 (declare (ignore msg)))
                               0
                               t)
    ;; we need an actor as watcher that is not the 'child' of the to be watched actor.
    (let* ((stopped-msg-received nil)
           (watcher (actor-of (ac:system (act:context cut))
                              :receive (lambda (msg)
                                         (case (car msg)
                                           (:stopped
                                            (progn
                                              (assert (eq cut (cdr msg)))
                                              (setf stopped-msg-received t))))))))
      (watch cut watcher)
      (is (= 1 (length (watchers cut))))
      (act-cell:stop cut)
      (is-true (await-cond 0.5 stopped-msg-received)))))

(test stop-actor--stopping-parent-stops-also-child
  "Tests that stopping a parent actor also the children are stopped."
  (with-fixture actor-fixture ((lambda (msg)
                                 (declare (ignore msg)))
                               0
                               t)
    (let ((child-actor (actor-of
                        cut
                        :receive (lambda (msg) (declare (ignore msg))))))
      (act-cell:stop cut)
      (is-true (await-cond 0.5
                 (notany
                  #'act-cell:running-p
                  (list cut child-actor)))))))

(test stop-actor--notifies-actor-context-as-watcher
  "Tests that when actor stops and an actor-context is a watcher, that it is notified about the stop."
  (with-fixture actor-fixture ((lambda (msg)
                                 (declare (ignore msg)))
                               0
                               t)
    (let ((child-actor (actor-of cut
                                 :receive (lambda (msg)
                                            (declare (ignore msg))))))
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
  (with-fixture actor-fixture ((lambda (msg)
                                 (sleep 0.2)
                                 (cond
                                   ((eq :add (car msg))
                                    (tell act-cell:*sender*
                                          (+ (second msg) (third msg))))))
                               0
                               nil)
    (let ((future (ask cut '(:add 0 5))))
      (is (eq :not-ready (fresult future)))
      (is-true (await-cond 1.0 (complete-p future)))
      (is (= 5 (fresult future))))))

(test single-actor--handle-ask-2
  "Test handle a composable asynchronous call. fcompleted before completion."
  (with-fixture actor-fixture ((lambda (msg)
                                 (sleep 0.5)
                                 (cond
                                   ((eq :add (car msg))
                                    (tell act-cell:*sender* (+ (second msg) (third msg))))))
                               0
                               nil)
    (let ((future (ask cut '(:add 0 5)))
          (fcompleted-result nil))
      (is (eq :not-ready (fresult future)))
      (fcompleted future (result)
        (setf fcompleted-result result))
      (is-true (await-cond 1.0 (complete-p future)))
      (is (= 5 (fresult future))))))

(test ask-s--shared--timeout
  "Tests for ask-s timeout."
  (with-fixture actor-fixture ((lambda (msg) (declare (ignore msg)))
                               0
                               t)
    (let* ((actor (actor-of cut
                            :receive (lambda (msg)
                                       (declare (ignore msg))
                                       (sleep 2)
                                       :my-result)))
           (result (ask-s actor "foo" :time-out 0.5)))
      (is (eq :handler-error (car result)))
      (is (typep (cdr result) 'utils:ask-timeout))
      (is (eq :stopped (ask-s actor :stop))))))

(test ask-s--shared--timeout-in-dispatcher
  "Tests for ask-s timeout."
  (with-fixture actor-fixture ((lambda (msg) (declare (ignore msg)))
                               0
                               t)
    (with-mocks ()
      (answer (disp:dispatch-async _ _)
        (progn
          (format t "Dispatch called...~%")
          (sleep 2)))
      (let* ((actor (actor-of cut
                              :receive (lambda (msg)
                                         (declare (ignore msg)))))
             (result (ask-s actor "foo" :time-out 0.5)))
        (is (eq :handler-error (car result)))
        (is (typep (cdr result) 'utils:ask-timeout))))))

;; (test ask--shared--timeout
;;   "Tests for ask timeout."
;;   (with-fixture actor-fixture ((lambda ())
;;                                0
;;                                t)
;;     (let* ((actor (actor-of cut
;;                     :receive (lambda (self msg state)
;;                                (declare (ignore self msg))
;;                                (sleep 2)
;;                                (cons :my-result state))))
;;            (future (ask actor "foo" :time-out 0.5)))
;;       (utils:await-cond 1.0 (complete-p future))
;;       (is (eq :handler-error (car (fresult future))))
;;       (format t "error: ~a~%" (cdr (fresult future)))
;;       (is (typep (cdr (fresult future)) 'utils:ask-timeout)))))

;; (test ask--shared--timeout--many
;;   "Tests creation of many actors and ask messages with timeouts."
;;   (with-fixture actor-fixture ((lambda ())
;;                                0
;;                                t)
;;     (let* ((the-context (context cut))
;;            (many-actors (loop :for i :from 1 :to 2000
;;                               :collect
;;                               (actor-of the-context
;;                                 :receive (lambda (self msg state)
;;                                            (declare (ignore self msg state))
;;                                            (sleep 2)))))
;;            (futures (mapcar (lambda (a) (ask a "Foo" :time-out 0.5)) many-actors)))
;;       (is-true (assert-cond
;;                 (lambda ()
;;                   (every
;;                    (lambda (n) (and (consp n)
;;                                (typep (cdr n) 'utils:ask-timeout)))
;;                    (mapcar #'fresult futures)))
;;                 0.7))
;;       (print (length futures)))))

;; (test ask-s--pinned--timeout
;;   "Tests for ask-s timeout."
;;   (with-fixture actor-fixture ((lambda (self msg state)
;;                                  (declare (ignore self msg))
;;                                  (sleep 2)
;;                                  (cons :my-result state))
;;                                0
;;                                nil)
;;     (let ((result (ask-s cut "foo" :time-out 0.5)))
;;       (is (eq :handler-error (car result)))
;;       (is (typep (cdr result) 'utils:ask-timeout))
;;       (is (eq :stopped (ask-s cut :stop))))))

;; (test ask--pinned--timeout
;;   "Tests for ask timeout."
;;   (with-fixture actor-fixture ((lambda (self msg state)
;;                                  (declare (ignore self msg))
;;                                  (sleep 2)
;;                                  (cons :my-result state))
;;                                0
;;                                nil)
;;     (let ((future (ask cut "foo" :time-out 0.5)))
;;       (utils:await-cond 1.0 (complete-p future))
;;       (is (eq :handler-error (car (fresult future))))
;;       (is (typep (cdr (fresult future)) 'utils:ask-timeout)))))

;; (test allow--no-reply--response
;;   "Tests to allow `:no-reply' for `tell', `ask-s' and `ask'"
;;   (with-fixture actor-fixture ((lambda (self msg state)
;;                                  (declare (ignore self msg))
;;                                  (if act-cell:*sender*
;;                                      (tell act-cell:*sender* :manual-reply))
;;                                  (cons :no-reply state))
;;                                0
;;                                t)
;;     (is-true (tell cut :foo))
;;     (is (eq :no-reply (ask-s cut :foo)))
;;     (let ((fut (ask cut :foo)))
;;       (assert-cond (lambda () (complete-p fut)) 1.0)
;;       (is (eq :manual-reply (fresult fut))))
;;   ))

;; (test become-and-unbecome-a-different-behavior
;;   "Test switching behaviors"
;;   (let* ((beh2 (lambda (self msg state)
;;                  (declare (ignore self msg))
;;                  (case msg
;;                    (:unbecome
;;                     (unbecome)))
;;                  (cons :behavior2 state)))
;;          (beh1 (lambda (self msg state)
;;                  (declare (ignore self msg))
;;                  (case msg
;;                    (:behavior2
;;                     (become beh2)))
;;                  (cons :behavior1 state)))
;;          (receive (lambda (self msg state)
;;                     (declare (ignore self msg))
;;                     (case msg
;;                       (:behavior1
;;                        (become beh1)))
;;                     (cons :receive state))))
;;     (with-fixture actor-fixture (receive 0 t)
;;       (is (eq :receive (ask-s cut :some)))
;;       (ask-s cut :behavior1)
;;       (is (eq :behavior1 (ask-s cut :some)))
;;       (ask-s cut :behavior2)
;;       (is (eq :behavior2 (ask-s cut :some)))
;;       (ask-s cut :unbecome)
;;       (is (eq :receive (ask-s cut :some))))))

;; (test actor-of
;;   "Tests the convenience actor-of macro"
;;   (let ((sys (asys:make-actor-system)))
;;     (unwind-protect
;;          (let* ((receive-fun (lambda (self msg state)
;;                                (declare (ignore self))
;;                                (when (string= "Foo" msg)
;;                                  (cons "Bar" state))))
;;                 (actor (actor-of sys :receive receive-fun))
;;                 (custom-actor (actor-of sys :receive receive-fun :type 'custom-actor)))
;;            (is (string= "Bar" (ask-s actor "Foo")))
;;            (is (string= "Bar" (ask-s custom-actor "Foo")))
;;            (is (typep custom-actor 'custom-actor)))
;;       (ac:shutdown sys))))

;; (test actor-of--create-in-right-context
;;   "Tests to conveniently specify three types as context: asys, ac and actor"
;;   (let ((sys (asys:make-actor-system)))
;;     (unwind-protect
;;          (let* ((receive-fun (lambda (self msg state)
;;                                (declare (ignore self))
;;                                (when (string= "Foo" msg)
;;                                  (cons "Bar" state))))
;;                 (system-actor (actor-of sys :name "foo1" :receive receive-fun))
;;                 (ac-actor (actor-of (act:context system-actor) :name "foo2" :receive receive-fun))
;;                 (actor-actor (actor-of ac-actor :name "foo3" :receive receive-fun)))
;;            (is (string= "Bar" (ask-s system-actor "Foo")))
;;            (is (string= "/user/foo1" (path system-actor)))
;;            (is (string= "Bar" (ask-s ac-actor "Foo")))
;;            (is (string= "/user/foo1/foo2" (path ac-actor)))
;;            (is (string= "Bar" (ask-s actor-actor "Foo")))
;;            (is (string= "/user/foo1/foo2/foo3" (path actor-actor))))
;;       (ac:shutdown sys))))

;; (test actor-of-macro--with-init-destroy-and-receive
;;   "Tests the macro by specifying the receive function and init function as plist parameters."
;;   (let ((sys (asys:make-actor-system)))
;;     (unwind-protect
;;          (let* ((init-called nil)
;;                 (destroy-called nil)
;;                 (actor (actor-of sys :name "foo"
;;                          :init (lambda (self)
;;                                  (declare (ignore self))
;;                                  (setf init-called t))
;;                          :receive (lambda (self msg state)
;;                                     (declare (ignore self))
;;                                     (when (string= "Foo" msg)
;;                                       (cons "Bar" state)))
;;                          :destroy (lambda (self)
;;                                     (declare (ignore self))
;;                                     (setf destroy-called t)))))
;;            (is (string= "Bar" (ask-s actor "Foo")))
;;            (is-true init-called)
;;            (is-false destroy-called)
;;            (ask-s actor :stop)
;;            (is-true destroy-called))
;;       (ac:shutdown sys))))
