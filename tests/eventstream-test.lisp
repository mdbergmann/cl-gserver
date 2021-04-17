(defpackage :cl-gserver.eventstream-test
  (:use :cl :fiveam :cl-gserver.eventstream :cl-gserver.utils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.eventstream-test)

(def-suite eventstream-tests
  :description "Tests for eventstream facility"
  :in cl-gserver.tests:test-suite)

(in-suite eventstream-tests)

(def-fixture test-ev ()
  (let* ((context (asys:make-actor-system))
         (cut (make-eventstream context)))
    (unwind-protect
         (&body)
      (ac:shutdown context)
      (sleep 0.1))))

(test make-eventstream
  "Creates an event stream."
  (is (make-eventstream
       (asys:make-actor-system '(:dispatchers (:num-shared-workers 0))))))

(test subscribe-ev--all
  "Subscribe to eventstream for all messages."
  (with-fixture test-ev ()
    (let ((act (act:make-actor (lambda ()))))
      (subscribe cut act)
      (is (= 1 (length (ev::subscribers cut))))
      (is (member act (ev::subscribers cut) :test #'eq :key #'car))
      ;; we did subscribe with no particular message, which means all
      (is (null (cdr (member act (ev::subscribers cut) :test #'eq)))))))

(test subscribe-ev--something
  "Subscribe with listening for something particular."
  (with-fixture test-ev ()
    (let ((act (act:make-actor (lambda ()))))
      (subscribe cut act "Foo")
      (is (string= "Foo" (cadar (member act (ev::subscribers cut) :test #'eq :key #'car)))))))

(test unsubscribe-ev
  "Test unsubscribing."
  (with-fixture test-ev ()
    (let ((act (act:make-actor (lambda ()))))
      (subscribe cut act)
      (unsubscribe cut act)
      (is (= 0 (length (ev::subscribers cut)))))))

(test publish-receive-ev
  "Publish a message to eventstream."
  (with-fixture test-ev ()
    (let* ((ev-received)
           (ev-listener (act:actor-of (context)
                         (lambda (self msg state)
                           (declare (ignore self state))
                           (setf ev-received msg)
                           (cons nil nil)))))
      ;; all
      (subscribe cut ev-listener)
      (publish cut "Foo")
      (is (assert-cond
           (lambda () (string= "Foo" ev-received)) 0.5))
      (unsubscribe cut ev-listener)
      (setf ev-received nil)

      ;; subscribe for string
      (subscribe cut ev-listener "Foo")
      (publish cut "Foo")
      (is (assert-cond
           (lambda () (string= "Foo" ev-received)) 0.5))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)

      ;; subscribe for string - no match
      (subscribe cut ev-listener "Foo")
      (publish cut "Bar")
      (sleep 0.5)
      (is (null ev-received))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)

      ;; subscribe for symbol
      (subscribe cut ev-listener 'foo)
      (publish cut 'foo)
      (is (assert-cond
           (lambda () (eq 'foo ev-received)) 0.5))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)

      ;; subscribe for symbol - no match
      (subscribe cut ev-listener 'foo)
      (publish cut 'bar)
      (sleep 0.5)
      (is (null ev-received))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)
      
      )))
