(defpackage :cl-gserver.agent.hash-test
  (:use :cl :fiveam :cl-gserver.agent.hash)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.agent.hash-test)

(def-suite agent.hash-tests
  :description "Tests for hash agent"
  :in cl-gserver.tests:test-suite)

(in-suite agent.hash-tests)

(def-fixture asys-fixture ()
  (let ((asys (asys:make-actor-system '(:dispatchers (:shared (:workers 1))))))
    (unwind-protect
         (&body)
      (ac:shutdown asys))))

(def-fixture agt ()
  (let ((cut (make-hash-agent nil)))
    (unwind-protect
         (&body)
      (agt:agent-stop cut))))

(test create
  "Tests creating a hash agent."
  (let ((cut (make-hash-agent nil)))
    (is-true cut)
    (agt:agent-stop cut)))

(test create--in-system
  "Tests creating a hash agent with providing an actor-context."
  (with-fixture asys-fixture ()
    (let ((cut (make-hash-agent asys)))
      (is-true cut))))

(test create--with-hash-table-keys
  "Tests creating an hash agent."
  (let ((cut (make-hash-agent nil :hash-table-args (list :test #'eq))))
    (is (str:containsp ":TEST EQ" (format nil "~a" cut)))
    (is-true cut)
    (agt:agent-stop cut)))

(test agent-puthash
  "Tests putting a value to hash agent -- private"
  (with-fixture agt ()
    (is (agthash::agent-puthash :key cut "my-value"))))

(test agent-gethash
  "Tests getting a key from hash-agent."
  (with-fixture agt ()
    (is (string= "my-value" (agthash::agent-puthash :key cut "my-value")))
    (is (string= "my-value" (agent-gethash :key cut)))))

(test agent-setf
  "Tests putting a value to hash agent."
  (with-fixture agt ()
    (is (string= "my-value" (setf (agent-gethash :key cut) "my-value")))
    (is (string= "my-value" (agent-gethash :key cut)))))

(test agent-remhash--exists
  "Tests removing a key. Returns T when the key existed."
  (with-fixture agt ()
    (setf (agent-gethash :foo cut) "my-value")
    (is (string= "my-value" (agent-gethash :foo cut)))
    (is-true (agent-remhash :foo cut))))
  
(test agent-remhash--not-exists
  "Tests removing a key. Returns NIL when the key not existed."
  (with-fixture agt ()
    (is-false (agent-remhash :foo cut))))

(test agent-clrhash
  "Tests clearing a hash table."
  (with-fixture agt ()
    (setf (agent-gethash :foo cut) "my-value")
    (is (string= "my-value" (agent-gethash :foo cut)))
    (is-true (agent-clrhash cut))
    (is-false (agent-gethash :foo cut))))

(test agent-dohash
  "Tests 'do' on hash table."
  (with-fixture agt ()
    (setf (agent-gethash :foo cut) 1)
    (setf (agent-gethash :bar cut) 2)
    (is-true (agent-dohash (lambda (hash-table)
                             (setf (gethash :foo hash-table) 10)
                             (setf (gethash :bar hash-table) 20)
                             (setf (gethash :buzz hash-table) 30))
                           cut))
    (is (= 10 (agent-gethash :foo cut)))
    (is (= 20 (agent-gethash :bar cut)))
    (is (= 30 (agent-gethash :buzz cut)))))
