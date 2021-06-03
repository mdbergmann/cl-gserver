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

(def-fixture test-system ()
  (let ((system (asys:make-actor-system)))
    (unwind-protect
         (&body)
      (ac:shutdown system))))

(def-fixture test-ev ()
  (let* ((system (asys:make-actor-system))
         (cut (make-eventstream system)))
    (unwind-protect
         (&body)
      (ac:shutdown system))))

(test make-eventstream
  "Creates an event stream."
  (let ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 0))))))
    (is (make-eventstream system))
    (ac:shutdown system)))

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

(test subscribe-works-directly-on-system-and-actor
  "Actor system and Actor implement the ev protocol"
  (with-fixture test-system ()
    (is (subscribe system (act:make-actor (lambda ()))))
    (is (subscribe (act:actor-of (system)
                        :receive (lambda ()))
                      (act:make-actor (lambda ()))))))

(test unsubscribe-ev
  "Test unsubscribing."
  (with-fixture test-ev ()
    (let ((act (act:make-actor (lambda ()))))
      (subscribe cut act)
      (unsubscribe cut act)
      (is (= 0 (length (ev::subscribers cut)))))))

(test unsubscribe-works-directly-on-system-and-actor
  "Actor system and Actor implement the ev protocol"
  (with-fixture test-system ()
    (is (unsubscribe system (act:make-actor (lambda ()))))
    (is (unsubscribe (act:actor-of (system)
                          :receive (lambda ()))
                      (act:make-actor (lambda ()))))))

(defclass my-class () ())
(defclass my-sub-class (my-class) ())

(test publish-receive-ev
  "Publish a message to eventstream."
  (with-fixture test-ev ()
    (let* ((ev-received)
           (ev-listener (act:actor-of (system)
                         :receive (lambda (self msg state)
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
      (subscribe cut ev-listener 'string)
      (publish cut "Foo")
      (is (assert-cond
           (lambda () (string= "Foo" ev-received)) 0.5))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)

      ;; subscribe for string - concrete
      (subscribe cut ev-listener "Foo")
      (publish cut "Foo")
      (is (assert-cond
           (lambda () (string= "Foo" ev-received)) 0.5))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)

      ;; subscribe for string - no match
      (subscribe cut ev-listener "Foo")
      (publish cut "Bar")
      (sleep 0.2)
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

      ;; subscribe for global symbol
      (subscribe cut ev-listener :foo)
      (publish cut :foo)
      (is (assert-cond
           (lambda () (eq :foo ev-received)) 0.5))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)

      ;; subscribe for symbol - no match
      (subscribe cut ev-listener 'foo)
      (publish cut 'bar)
      (sleep 0.2)
      (is (null ev-received))
      (unsubscribe cut ev-listener)      
      (setf ev-received nil)

      ;; subscribe for symbol - class
      (subscribe cut ev-listener 'my-class)
      (let ((obj (make-instance 'my-class)))
        (publish cut obj)
        (is (assert-cond
             (lambda () (eq obj ev-received)) 0.5)))
      (unsubscribe cut ev-listener)
      (setf ev-received nil)

      ;; subscribe for symbol - sub-class
      (subscribe cut ev-listener 'my-sub-class)
      (let ((obj (make-instance 'my-class)))
        (publish cut obj)
        (sleep 0.2)
        (is (null ev-received)))
      (unsubscribe cut ev-listener)
      (setf ev-received nil)

      ;; subscribe for symbol - parent-class
      (subscribe cut ev-listener 'my-class)
      (let ((obj (make-instance 'my-sub-class)))
        (publish cut obj)
        (is (assert-cond
             (lambda () (eq obj ev-received)) 0.5)))
      (unsubscribe cut ev-listener)
      (setf ev-received nil)

      ;; subscribe for list
      (subscribe cut ev-listener 'cons)
      (publish cut '(1 2 3))
      (is (assert-cond
           (lambda () (equalp '(1 2 3) ev-received)) 0.5))
      (unsubscribe cut ev-listener)
      (setf ev-received nil)
      
      ;; subscribe for list - structure compare
      (subscribe cut ev-listener '(1 2 3))
      (publish cut '(1 2 3))
      (is (assert-cond
           (lambda () (equalp '(1 2 3) ev-received)) 0.5))
      (unsubscribe cut ev-listener)
      (setf ev-received nil)

      ;; subscribe for list - structure compare - no match
      (subscribe cut ev-listener '(1 2))
      (publish cut '(1 2 3))
      (sleep 0.2)
      (is (null ev-received))
      (unsubscribe cut ev-listener)
      (setf ev-received nil))))

(test publish-works-directly-on-system-and-actor
  "Actor system and Actor implement the ev protocol"
  (with-fixture test-system ()
    (is (publish system "Foo"))
    (is (publish (act:actor-of (system)
                      :receive (lambda ()))
                    "Foo"))))

(defstruct counters
  (string 0)
  (stringp 0)
  (list 0)
  (listp 0))
(test integration-like-test
  "Integration - and a better overall example. Eating our own dogfood."
  (with-fixture test-system ()
    (let* ((receive-fun (lambda (self msg state)
                          (declare (ignore self))
                          (cond
                            ((stringp msg)
                             (progn
                               (incf (counters-stringp state))
                               (when (string= "Awaited message1" msg)
                                 (incf (counters-string state)))))
                            ((listp msg)
                             (progn
                               (incf (counters-listp state))
                               (when (equalp '("foo" "bar" "buzz") msg)
                                 (incf (counters-list state))))))
                          (cons t state)))
           (actor1 (act:actor-of (system "actor 1")
                     :state (make-counters)
                     :init (lambda (self)
                             (subscribe system self "Awaited message1")
                             (subscribe system self 'cons))
                     :receive receive-fun))
           (actor2 (act:actor-of (system "actor 2")
                     :state (make-counters)
                     :init (lambda (self)
                             (subscribe system self '("foo" "bar" "buzz"))
                             (subscribe system self 'string))
                     :receive receive-fun)))
      (loop :for i :from 0 :to 999
            :when (oddp i)
              :do (progn (publish system "Awaited message1")
                         (publish system "some other message"))
            :when (evenp i)
              :do (progn
                    (publish system '("foo" "bar" "buzz"))
                    (publish system '(1 2 3))))

      (is (assert-cond (lambda () (and
                              (= (+ (counters-string (act-cell:state actor1))
                                    (counters-string (act-cell:state actor2))) 1000)
                              (= (+ (counters-stringp (act-cell:state actor1))
                                    (counters-stringp (act-cell:state actor2))) 1500)
                              (= (+ (counters-list (act-cell:state actor1))
                                    (counters-list (act-cell:state actor2))) 1000)
                              (= (+ (counters-listp (act-cell:state actor1))
                                    (counters-listp (act-cell:state actor2))) 1500))) 2))
      (format t "~%counter string1: ~a~%" (counters-string (act-cell:state actor1)))
      (format t "~%counter string2: ~a~%" (counters-string (act-cell:state actor2)))
      (format t "counter list1: ~a~%" (counters-list (act-cell:state actor1)))
      (format t "counter list2: ~a~%" (counters-list (act-cell:state actor2)))
      (format t "counter listp1: ~a~%" (counters-listp (act-cell:state actor1)))
      (format t "counter listp2: ~a~%" (counters-listp (act-cell:state actor2)))
      (format t "counter stringp1: ~a~%" (counters-stringp (act-cell:state actor1)))
      (format t "counter stringp2: ~a~%" (counters-stringp (act-cell:state actor2))))))
