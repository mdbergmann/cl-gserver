(defpackage :cl-gserver.actor-test
  (:use :cl :fiveam :cl-gserver.actor :cl-gserver.future)
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

(def-fixture actor-fixture (receive-fun state)
  (defclass test-actor (actor) ())
  (let ((cut (make-actor receive-fun
                         :state state)))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt))
    (unwind-protect
         (&body)
      (tell cut :stop))))

(test make-actor--has-no-msgbox-and-actor-context
  "Test constructor. actor should not have msgbox and attached actor-context."

  (let ((actor (make-actor (lambda (self msg state)
                             (declare (ignore self msg state))
                             nil))))
    (is (null (act-cell:msgbox actor)))
    (is (null (context actor)))))

(test make-actor--with-msgbox
  "Test contructor and attach a msgbox manually."

  (let ((cut (make-actor (lambda (actor message current-state)
                             (declare (ignore actor))
                             (cond
                               ((string= message "foo") (cons 1 1))
                               ((string= message "bar") (cons 5 5))
                               ((string= message "get") (cons current-state current-state)))))))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt))
    (is (not (null cut)))
    (is (eq t (tell cut "foo")))
    (is (eq t (assert-cond (lambda () (= 1 (ask cut "get"))) 1)))
    (is (= 5 (ask cut "bar")))
    (is (= 5 (ask cut "get")))
    (ask cut :stop)))

(test actor-of--from-existing-actor-context
  "Tests that a new 'child' actor can be created from an actor context."
  (with-fixture actor-fixture ((lambda (self message current-state)
                                 (declare (ignore self message current-state)))
                               0)
    ;; attach actor-context - this is usually done when the actor was created from the actor-system
    (setf (act:context cut)
          (ac:make-actor-context (asys:make-actor-system :shared-dispatcher-workers 0)))
    (let ((child-actor (ac:actor-of (act:context cut)
                                    (lambda () (make-actor (lambda (self msg state)
                                                        (declare (ignore self msg state))))))))
      (is (not (null child-actor)))
      (is (not (eq (act:context child-actor) (act:context cut))))
      (is (eq (ac:system (act:context child-actor)) (ac:system (act:context cut))))
      (is (eq child-actor (first (ac:all-actors (act:context cut))))))))

(test stop-actor--stoping-parent-stops-also-child
  "Tests that stopping a parent actor also the children are stopped."
  (with-fixture actor-fixture ((lambda (self message current-state)
                                 (declare (ignore self message current-state)))
                               0)
    ;; attach actor-context
    (setf (act:context cut)
          (ac:make-actor-context (asys:make-actor-system :shared-dispatcher-workers 0)))
    (let ((child-actor (ac:actor-of (act:context cut)
                                    (lambda () (make-actor (lambda (self msg state)
                                                        (declare (ignore self msg state))))))))
      (act-cell:stop cut)
      (is (assert-cond (lambda ()
                         (notany
                          #'act-cell:running-p
                          (list cut child-actor))) 1))
      )))

(test single-actor--handle-async-ask
  "Tests the async ask function."

  (with-fixture actor-fixture ((lambda (self message current-state)
                                 (declare (ignore self))
                                 (sleep 0.2)
                                 (cond
                                   ((eq :add (car message))
                                    (cons (+ (second message) (third message)) current-state))))
                               0)
    (let ((future (async-ask cut '(:add 0 5))))
      (is (eq :not-ready (get-result future)))
      (is (eq t (assert-cond (lambda () (complete-p future)) 1)))
      (is (= 5 (get-result future))))))


(test single-actor--handle-async-ask-2
  "Test handle a composable asynchronous call. on-completed before completion."

  (with-fixture actor-fixture ((lambda (self message current-state)
                                        (declare (ignore self))
                                        (sleep 0.5)
                                        (cond
                                          ((eq :add (car message))
                                           (cons (+ (second message) (third message)) current-state))))
                               0)
    (let ((future (async-ask cut '(:add 0 5)))
          (on-completed-result nil))
      (is (eq :not-ready (get-result future)))
      (on-completed future (lambda (result)
                             (setf on-completed-result result)))
      (is (eq t (assert-cond (lambda () (complete-p future)) 1)))
      (is (= 5 (get-result future))))))


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
  (run! 'stop-actor--stoping-parent-stops-also-child)
  (run! 'single-actor--handle-async-ask)
  (run! 'single-actor--handle-async-ask-2)
  ;;(run! 'with-actor-macro)
  )
