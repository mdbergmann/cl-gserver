(defpackage :sento.dispatcher-test
  (:use :cl :fiveam :sento.dispatcher :sento.actor)
  (:shadow #:! #:?)
  (:import-from #:miscutils
                #:await-cond)
  (:import-from #:timeutils
                #:ask-timeout))

(in-package :sento.dispatcher-test)

(def-suite dispatcher-tests
  :description "Tests for dispatcher"
  :in sento.tests:test-suite)

(in-suite dispatcher-tests)

(def-fixture test-context ()
  (let ((context (asys:make-actor-system '(:dispatchers (:shared (:workers 0))))))
    (unwind-protect
         (&body)
      (ac:shutdown context))))

(def-fixture test-dispatcher (num-workers)
  "A dispatcher with NUM-WORKERS workers in its own context, stopped afterwards."
  (let* ((context (asys:make-actor-system '(:dispatchers (:shared (:workers 0)))))
         (cut (make-test-dispatcher num-workers context "foo")))
    (unwind-protect
         (&body)
      (stop cut)
      (ac:shutdown context))))

(defun make-test-dispatcher (num-workers context ident)
  (make-dispatcher (ac:make-actor-context context)
                   ident
                   :workers num-workers))

(defun looper ()
  (loop :for i :from 1 :to 5 :sum i))

(test create-dispatcher
  "Checks creating a dispatcher"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 1 context "foo")))
      (is (not (null cut)))
      (is (string= "foo" (identifier cut)))
      (is (= *default-throughput* (throughput cut)))
      (stop cut))))

(test create-dispatcher--with-config
  "Tests creating a dispatcher with a custom config."
  (with-fixture test-context ()
    (let ((cut (apply #'make-dispatcher context :foo '(:workers 0 :strategy :round-robin :throughput 20))))
      (is (eq :foo (identifier cut)))
      (is (= 0 (length (workers cut))))
      (is (= 20 (throughput cut)))
      (stop cut))))

(test create-the-workers
  "Checks that the workers are created as actors"
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 4 context "foo")))
      (is (= 4 (length (workers cut))))
      (is (every (lambda (w) (typep w 'dispatch-worker)) (workers cut)))
      (stop cut))))

(test dispatch-to-worker
  "Tests the dispatching to a worker"
  (with-fixture test-dispatcher (1)
    (is (= 15 (dispatch cut (list #'looper))))))

(test dispatch--with-args
  "The dispatched function is applied to the arguments that follow it."
  (with-fixture test-dispatcher (1)
    (is (= 6 (dispatch cut (list #'+ 1 2 3))))))

(test dispatch--bypasses-actor-message-handling
  "A dispatched function runs straight from the worker's message-box, not
through the actor-cell message handling: `*self*' is not bound to the worker."
  (with-fixture test-dispatcher (1)
    (is-false (dispatch cut (list (lambda () *self*))))))

(test dispatch--handler-error-on-condition
  "A condition signaled by the dispatched function is returned as `handler-error',
and the worker keeps working afterwards."
  (with-fixture test-dispatcher (1)
    (let ((result (dispatch cut (list (lambda () (error "boom"))))))
      (is (eq :handler-error (car result)))
      (is (typep (cdr result) 'simple-error)))
    (is (= 2 (dispatch cut (list #'1+ 1))))))

(test dispatch--time-out
  "A dispatched function that takes longer than the ask-s time-out yields an
`ask-timeout' handler-error."
  (with-fixture test-dispatcher (1)
    (let ((result (act:ask-s (first (workers cut))
                             (list (lambda () (sleep 1)))
                             :time-out 0.2)))
      (is (eq :handler-error (car result)))
      (is (typep (cdr result) 'ask-timeout)))))

(test dispatch--stopped-dispatcher
  "Dispatching to a stopped dispatcher returns `:stopped' instead of blocking."
  (with-fixture test-context ()
    (let ((cut (make-test-dispatcher 1 context "foo")))
      (stop cut)
      (is (eq :stopped (dispatch cut (list #'looper))))
      (is (eq :stopped (dispatch-async cut (list #'looper)))))))

(test dispatch-async--executes-on-worker
  "`dispatch-async' returns `T' right away and the function runs on a worker thread."
  (with-fixture test-dispatcher (1)
    (let ((ran-on nil)
          (caller (bt2:current-thread)))
      (is (eq t (dispatch-async cut (list (lambda ()
                                            (setf ran-on (bt2:current-thread)))))))
      (is-true (await-cond 0.5 ran-on))
      (is (not (eq caller ran-on))))))

(test dispatch-async--no-workers
  "`dispatch-async' on a dispatcher without workers returns `nil'."
  (with-fixture test-dispatcher (0)
    (is-false (dispatch-async cut (list #'looper)))))
