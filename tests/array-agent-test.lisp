(defpackage :cl-gserver.agent.array-test
  (:use :cl :fiveam :cl-gserver.agent.array)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.agent.array-test)

(def-suite agent.array-tests
  :description "Tests for array agent"
  :in cl-gserver.tests:test-suite)

(in-suite agent.array-tests)

(def-fixture asys-fixture ()
  (let ((asys (asys:make-actor-system '(:dispatchers (:shared (:workers 1))))))
    (unwind-protect
         (&body)
      (ac:shutdown asys))))

(def-fixture agt (arr err-fun)
  (let ((cut (make-array-agent nil :initial-array arr :error-fun err-fun)))
    (unwind-protect
         (&body)
      (agt:agent-stop cut))))

(test create
  "Tests creating a array agent."
  (let ((cut (make-array-agent nil :initial-array #())))
    (is-true cut)
    (agt:agent-stop cut)))

(test create--in-system
  "Tests creating a array agent with providing an actor-context."
  (with-fixture asys-fixture ()
    (let ((cut (make-array-agent asys :initial-array #())))
      (is-true cut))))

(test agent-elt
  "Tests retrieve element."
  (with-fixture agt (#(10 20) nil)
    (is (= 10 (agent-elt 0 cut)))
    (is (= 20 (agent-elt 1 cut)))))

(test agent-setf
  "Tests setf'ing a value to array."
  (with-fixture agt (#(10 20) nil)
    (is (= 11 (setf (agent-elt 0 cut) 11)))))

(test agent-setf--err-no-index
  "Tests setf'ing a value to array whos index doesn't exist.
While the test succeeds, error-fun is called."
  (let* ((err-cond)
         (err-fun (lambda (err) 
                    (setf err-cond err))))
   (with-fixture agt (#() err-fun)
     (is (= 11 (setf (agent-elt 0 cut) 11)))
     (is (utils:assert-cond (lambda ()
                              (not (null err-cond))) 0.5)))))

(test agent-push
  "Tests pushing new value."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t) nil)
    (is-true (agent-push 1 cut))
    (is-true (agent-push 2 cut))
    (is (= 1 (agent-elt 0 cut)))
    (is (= 2 (agent-elt 1 cut)))))

(test agent-push--err
  "Tests pushing new value with calling err-fun."
  (let* ((err-cond)
         (err-fun (lambda (err)
                    (setf err-cond err))))
    ;; no fill-pointer
    (with-fixture agt ((make-array 0 :adjustable t) err-fun)
      (agent-push 1 cut)
      (is (utils:assert-cond (lambda () (not (null err-cond))) 0.5)))))

(test agent-push-and-getidx
  "Tests pushing with returning the new index."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t) nil)
    (is (= 0 (agent-push-and-getidx 1 cut)))
    (is (= 1 (agent-push-and-getidx 2 cut)))))

(test agent-push-and-getidx--err
  "Tests pushing with returning an error. Missing fill-pointer here."
  (with-fixture agt ((make-array 0 :adjustable t) nil)
    (is (typep (agent-push-and-getidx 1 cut) 'error))))

(test agent-pop
  "Tests poping value."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t) nil)
    (is-true (agent-push 1 cut))
    (is-true (agent-push 2 cut))
    (is (= 2 (agent-pop cut)))
    (is (= 1 (agent-pop cut)))))

(test agent-pop--err
  "Tests poping empty array."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t) nil)
    (is (typep (agent-pop cut) 'error))))

(test agent-delete
  "Tests deleting an item."
  (with-fixture agt ((make-array 0 :adjustable t :fill-pointer t) nil)
    (is-true (agent-push "foo" cut))
    (is (string= "foo" (agent-elt 0 cut)))
    (is-true (agent-delete "foo" cut :test #'string=))
    (is (typep (agent-elt 0 cut) 'error))))

(test agent-delete--err
  "Tests deleting an item with calling err-fun.
`delete` does raise an error, at least not that I would know.
So this test just does nothing really."
  (let* ((err-cond)
         (err-fun (lambda (err)
                    (setf err-cond err))))
    (with-fixture agt (#() err-fun)
      (is-true (agent-delete "foo" cut :test #'string=))
      (is (utils:assert-cond (lambda () (null err-cond)) 0.5)))))

(test agent-doarray
  "Tests running arbitrary array operations on the agent."
  (with-fixture agt (#() nil)
    (is-true (agent-doarray (lambda (array)
                              (declare (ignore array))
                              (remove-if #'evenp #(1 2 3 4 5)))
                            cut))
    (is (= 1 (agent-elt 0 cut)))
    (is (= 3 (agent-elt 1 cut)))
    (is (= 5 (agent-elt 2 cut)))))
