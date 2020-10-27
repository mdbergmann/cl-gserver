(defpackage :cl-gserver.future-test
  (:use :cl :fiveam :cl-gserver.future)
  (:import-from #:utils
                #:assert-cond)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.future-test)

(def-suite future-tests
  :description "Future tests"
  :in cl-gserver.tests:test-suite)

(in-suite future-tests)

(test create-future
  "Creates a future"

  (is (typep (make-future nil) 'future))
  (is (typep (make-future (lambda (promise-fun)
                            (declare (ignore promise-fun)) nil)) 'future)))

(test provide-promise
  "Executes future and provides promise"

  (let ((future (make-future (lambda (promise-fun)
                               (funcall promise-fun "fulfilled")))))
    (is (eq t (complete-p future)))
    (is (string= "fulfilled" (get-result future)))))

(test on-complete-callback
  "Executes future and get result via on-complete callback."

  (let ((future (make-future (lambda (promise-fun)
                               (funcall promise-fun "fulfilled"))))
        (completed-value nil))
    (on-completed future (lambda (value) (setf completed-value value)))
    (is (string= "fulfilled" completed-value))))

(test complete-with-delay
  "Test the completion with on-completed callback with a delayed execution."

  (let ((future (make-future (lambda (promise-fun)
                               (bt:make-thread (lambda ()
                                                 (sleep 0.5)
                                                 (funcall promise-fun "fulfilled"))))))
        (completed-value))
    (is (eq :not-ready (get-result future)))
    (on-completed future (lambda (value) (setf completed-value value)))
    (is (eq t (assert-cond (lambda () (string= "fulfilled" completed-value)) 1)))
    )
  )

(defun run-tests ()
  (run! 'create-future)
  (run! 'provide-promise)
  (run! 'on-complete-callback)
  (run! 'complete-with-delay))
