(defpackage :sento.router-test
  (:use :cl :fiveam :cl-mock :sento.router)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.router-test)

(def-suite router-tests
  :description "Tests for router"
  :in sento.tests:test-suite)

(in-suite router-tests)

(def-fixture system-fixture ()
  (let ((system (asys:make-actor-system)))
    (unwind-protect
         (&body)
      (ac:shutdown system))))

(defun make-actor (context)
  (ac:actor-of context
    :receive (lambda (msg) msg))) ; just returns msg

(test router--create
  "Creates a plain router"
  (with-fixture system-fixture ()
    (is (not (null (make-router))))
    (is (typep (make-router) 'router))
    (is (functionp (strategy-fun (make-router))))))

(test router--add-routee
  "Tests adding routees (actors)"
  (with-fixture system-fixture ()
    (let ((cut (make-router)))
      (dotimes (i 5)
        (add-routee cut (make-actor system)))
      (is (= 5 (length (routees cut)))))))

(test router--provide-routees-at-contructor
  "Provide routees when constructing."
  (with-fixture system-fixture ()
    (let ((cut (make-router :routees (list
                                      (make-actor system)
                                      (make-actor system)
                                      (make-actor system)))))
      (is (= 3 (length (routees cut)))))))

(test router--stop
  "Stopping router stops routees."
  (with-fixture system-fixture ()
    (with-mocks ()
      (answer (ac:actor-of _ create-fun) (funcall create-fun))
      (answer (act-cell:stop _) t)
    
      (let ((cut (make-router)))
        (dotimes (i 2)
          (add-routee cut (make-actor system)))
        (is (= 2 (length (routees cut))))
        (is (equalp '(t t) (stop cut)))
        (is (= 2 (length (invocations 'act-cell:stop))))))))

(test router--routees--returns-fresh-list
  "The routee list handed out is a copy: changing it does not touch the router."
  (with-fixture system-fixture ()
    (let ((cut (make-router :routees (list (make-actor system) (make-actor system)))))
      (setf (cdr (routees cut)) nil)
      (is (= 2 (length (routees cut)))))))

(test router--no-routees
  "`tell', `ask-s' and `ask' return `nil' when the router has no routees."
  (with-fixture system-fixture ()
    (let ((cut (make-router)))
      (is-false (tell cut "Foo"))
      (is-false (ask-s cut "Foo"))
      (is-false (ask cut "Foo")))))

(test router--strategy-index--used-for-every-message
  "The strategy chooses the routee per message: a strategy that always picks the
second routee makes every message land there."
  (with-fixture system-fixture ()
    (let* ((first-count 0)
           (second-count 0)
           (cut (make-router :strategy (lambda (len) (declare (ignore len)) 1)
                             :routees (list
                                       (ac:actor-of system
                                                    :receive (lambda (msg)
                                                               (declare (ignore msg))
                                                               (incf first-count)))
                                       (ac:actor-of system
                                                    :receive (lambda (msg)
                                                               (declare (ignore msg))
                                                               (incf second-count)))))))
      (loop :repeat 5 :do (ask-s cut "Foo"))
      (is (= 0 first-count))
      (is (= 5 second-count)))))

(test router--tell
  "Tests 'tell' on the router which forwards to an actor chosen by the strategy."
  (with-fixture system-fixture ()
    (let ((cut (make-router :routees (list
                                      (make-actor system)
                                      (make-actor system)))))
      (is (equalp (loop :repeat 5
                        :collect t)
                  (loop :repeat 5
                        :collect (tell cut "Foo")))))))

(test router--ask-s
  "Tests 'ask-s' on the router which forwards to an actor chosen by the strategy."
  (with-fixture system-fixture ()
    (let ((cut (make-router :routees (list
                                      (make-actor system)
                                      (make-actor system)))))
      (is (equalp (loop :repeat 5
                        :collect "Foo")
                  (loop :repeat 5
                        :collect (ask-s cut "Foo")))))))

(test router--ask
  "Tests 'ask' on the router which forwards to an actor chosen by the strategy."
  (with-fixture system-fixture ()
    (let* ((receive-fun (lambda (msg)
                          (declare (ignore msg))))
           (cut (make-router :routees (list
                                       (ac:actor-of system :receive receive-fun)
                                       (ac:actor-of system :receive receive-fun)))))
      (is (every (lambda (x) (typep x 'future:future))
                 (loop :repeat 5
                       :collect (ask cut "Foo")))))))

(test router--round-robin-strategy
  "Tests the router round-robin strategy"
  (with-fixture system-fixture ()
    (let ((rr-strategy (router::make-round-robin-strategy)))
      (is (= 1 (funcall rr-strategy 3)))
      (is (= 2 (funcall rr-strategy 3)))
      (is (= 0 (funcall rr-strategy 3)))
      (is (= 1 (funcall rr-strategy 3)))
      (is (= 2 (funcall rr-strategy 3)))
      (is (= 0 (funcall rr-strategy 3))))))

(test router--tell--with-round-robin-strategy
  "Tests 'tell' with round-robin strategy"
  (with-fixture system-fixture ()
    (let ((cut (make-router :strategy :round-robin
                            :routees (list
                                      (make-actor system)
                                      (make-actor system)))))
      (is (equalp (loop :repeat 5
                        :collect t)
                  (loop :repeat 5
                        :collect (tell cut "Foo")))))))
