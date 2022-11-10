(defpackage :sento.future-test
  (:use :cl :fiveam :sento.future)
  (:import-from #:utils
                #:assert-cond)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.future-test)

(def-suite future-tests
  :description "Future tests"
  :in sento.tests:test-suite)

(in-suite future-tests)

(test create-future
  "Creates a future"

  (is (typep (make-future nil) 'future))
  (is (typep (make-future (lambda (resolve-fun)
                            (declare (ignore resolve-fun)) nil))
             'future)))

(test provide-promise
  "Executes future and provides promise"

  (let ((future (make-future (lambda (resolve-fun)
                               (funcall resolve-fun "fulfilled")))))
    (is (eq t (complete-p future)))
    (is (string= "fulfilled" (get-result future)))))

(test on-complete-callback
  "Executes future and get result via on-complete callback."

  (let ((future (make-future (lambda (resolve-fun)
                               (funcall resolve-fun "fulfilled"))))
        (completed-value nil))
    (on-completed future (lambda (value) (setf completed-value value)))
    (is (string= "fulfilled" completed-value))))

(test complete-with-delay
  "Test the completion with on-completed callback with a delayed execution."

  (let ((future (make-future (lambda (resolve-fun)
                               (bt:make-thread (lambda ()
                                                 (sleep 0.5)
                                                 (funcall resolve-fun "fulfilled"))))))
        (completed-value))
    (is (eq :not-ready (get-result future)))
    (on-completed future (lambda (value) (setf completed-value value)))
    (is (eq t (assert-cond (lambda () (string= "fulfilled" completed-value)) 1)))))

(test mapping-futures
  "Tests mapping futures"
  (flet ((future-generator (x)
           (make-future (lambda (resolve-fun)
                          (funcall resolve-fun (+ x 1))))))
    (let ((future (fmap (future-generator 0)
                        (lambda (completed-value)
                          (fmap (future-generator completed-value)
                                (lambda (completed-value)
                                  (fmap (future-generator completed-value)
                                        (lambda (completed-value)
                                          completed-value))))))))
      (is-true (assert-cond (lambda ()
                              (= 3 (get-result future)))
                            1)))))

(run! 'mapping-future)
