(defpackage :cl-gserver.actor-test
  (:use :cl :fiveam :cl-gserver.actor :cl-gserver.future)
  (:import-from #:cl-gserver.actor-cell-test
                #:assert-cond)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.actor-test)

(def-suite actor-tests
  :description "actor tests"
  :in cl-gserver.tests:test-suite)

(in-suite actor-tests)

(def-fixture actor-fixture (receive-fun after-start-fun state)
  (defclass test-actor (actor) ())
  (let ((cut (make-instance 'actor
                            :state state
                            :receive-fun receive-fun
                            :after-start-fun after-start-fun
                            :msgbox (make-instance 'mesgb:message-box-bt))))
    (unwind-protect
         (&body)
      (tell cut :stop))))

(test create-alone-actor
  "Test a subclass of actor."

  (with-fixture actor-fixture ((lambda (actor message current-state)
                                 (declare (ignore actor))
                                 (cond
                                   ((string= message "foo") (cons 1 1))
                                   ((string= message "bar") (cons 5 5))
                                   ((string= message "get") (cons current-state current-state))))
                               nil
                               0)
    (is (not (null cut)))
    (is (eq t (tell cut "foo")))
    (is (eq t (assert-cond (lambda () (= 1 (ask cut "get"))) 1)))
    (is (= 5 (ask cut "bar")))
    (is (= 5 (ask cut "get")))))


(test run-after-start-fun
  "Tests the execution of `after-start-fun'"
  
  (with-fixture actor-fixture ((lambda (self message current-state)
                                 (declare (ignore self message))
                                 (cons current-state current-state))
                               (lambda (self state)
                                 (declare (ignore state))
                                 (with-slots (act-cell:state) self
                                   (setf act-cell:state "after-start-fun-executed")))
                               0)
    (is (string= "after-start-fun-executed" (ask cut :foo)))))

(test single-actor--handle-async-ask
  "Tests the async ask function."

  (with-fixture actor-fixture ((lambda (self message current-state)
                                 (declare (ignore self))
                                 (sleep 0.2)
                                 (cond
                                   ((eq :add (car message))
                                    (cons (+ (second message) (third message)) current-state))))
                               nil
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
                               nil
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
  (run! 'create-alone-actor)
  (run! 'run-after-start-fun)
  (run! 'single-actor)
  (run! 'single-actor--handle-async-ask)
  (run! 'single-actor--handle-async-ask-2)
  ;;(run! 'with-actor-macro)
  )
