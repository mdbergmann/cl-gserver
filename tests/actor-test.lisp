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
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box-bt))
    (unwind-protect
         (&body)
      (tell cut :stop))))

(test make-actor--has-no-msgbox-and-system
  "Test constructor. actor should not have msgbox and attached system by default."

  (let ((actor (make-actor (lambda (self msg state)
                             (declare (ignore self msg state))
                             nil))))
    (is (null (act-cell:msgbox actor)))
    (is (null (act-cell:system actor)))))

(test make-actor--with-msgbox
  "Test contructor and attach a msgbox manually."

  (let ((cut (make-actor (lambda (actor message current-state)
                             (declare (ignore actor))
                             (cond
                               ((string= message "foo") (cons 1 1))
                               ((string= message "bar") (cons 5 5))
                               ((string= message "get") (cons current-state current-state)))))))
    (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box-bt))
    (is (not (null cut)))
    (is (eq t (tell cut "foo")))
    (is (eq t (assert-cond (lambda () (= 1 (ask cut "get"))) 1)))
    (is (= 5 (ask cut "bar")))
    (is (= 5 (ask cut "get")))
    (ask cut :stop)))

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
  (run! 'make-actor--has-no-msgbox-and-system)
  (run! 'make-actor--with-msgbox)
  (run! 'single-actor--handle-async-ask)
  (run! 'single-actor--handle-async-ask-2)
  ;;(run! 'with-actor-macro)
  )
