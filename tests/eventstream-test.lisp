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
  (let ((system (asys:make-actor-system '(:dispatchers (:num-shared-workers 0)))))
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
    (is (ev:subscribe system (act:make-actor (lambda ()))))
    (is (ev:subscribe (act:actor-of (system)
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
    (is (ev:unsubscribe system (act:make-actor (lambda ()))))
    (is (ev:unsubscribe (act:actor-of (system)
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
    (is (ev:publish system "Foo"))
    (is (ev:publish (act:actor-of (system)
                          :receive (lambda ()))
                      "Foo"))))

(test integration-like-test
  "Integration - and a better overall example"
  (with-fixture test-system ()
    (let* ((counter-string 0)
           (counter-list 0)
           (counter-listp 0)
           (counter-stringp 0)
           (inc-counter-string (lambda () (stmx:atomic (incf counter-string))))
           (inc-counter-stringp (lambda () (stmx:atomic (incf counter-stringp))))
           (inc-counter-list (lambda () (stmx:atomic (incf counter-list))))
           (inc-counter-listp (lambda () (stmx:atomic (incf counter-listp))))
           (receive-fun (lambda (self msg state)
                          (declare (ignore self))
                          (cond
                            ((stringp msg)
                             (progn
                               (funcall inc-counter-stringp)
                               (when (string= "Awaited message1" msg)
                                 (funcall inc-counter-string))))
                            ((listp msg)
                             (progn
                               (funcall inc-counter-listp)
                               (when (equalp '("foo" "bar" "buzz") msg)
                                 (funcall inc-counter-list)))))
                          (cons t state)))
           (actor1 (act:actor-of (system "actor 1")
                     :init (lambda (self)
                             (subscribe system self "Awaited message1")
                             (subscribe system self 'cons))
                     :receive receive-fun))
           (actor2 (act:actor-of (system "actor 2")
                     :init (lambda (self)
                             (subscribe system self '("foo" "bar" "buzz"))
                             (subscribe system self 'string))
                     :receive receive-fun)))
      (declare (ignore actor1 actor2))

      (loop :for i :from 0 :to 999
            :when (oddp i)
              :do (progn (publish system "Awaited message1")
                         (publish system "some other message"))
            :when (evenp i)
              :do (progn
                    (publish system '("foo" "bar" "buzz"))
                    (publish system '(1 2 3))))

      (is (assert-cond (lambda () (and
                              (= counter-string 1000)
                              (= counter-stringp 1500)
                              (= counter-list  1000)
                              (= counter-listp 1500))) 2))
      (format t "counter string: ~a~%" counter-string )
      (format t "counter list: ~a~%" counter-list)
      (format t "counter listp: ~a~%" counter-listp)
      (format t "counter stringp: ~a~%" counter-stringp))))
