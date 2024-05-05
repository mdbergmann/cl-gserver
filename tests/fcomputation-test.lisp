(defpackage :sento.future-test
  (:use :cl :fiveam :binding-arrows :sento.future)
  (:import-from #:miscutils
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
             'future))
  (is (futurep (make-future nil))))

(test provide-promise
  "Executes future and provides promise"

  (let ((future (make-future (lambda (resolve-fun)
                               (funcall resolve-fun "fulfilled")))))
    (is (eq t (complete-p future)))
    (is (string= "fulfilled" (fresult future)))))

(test on-complete-callback
  "Executes future and get result via on-complete callback."

  (let ((future (make-future (lambda (resolve-fun)
                               (funcall resolve-fun "fulfilled"))))
        (completed-value nil))
    (fcompleted future (value) (setf completed-value value))
    (is (string= "fulfilled" completed-value))))

(test complete-with-delay
  "Test the completion with fcompleted callback with a delayed execution."

  (let ((future (make-future (lambda (resolve-fun)
                               (bt2:make-thread
                                (lambda ()
                                  (sleep 0.5)
                                  (funcall resolve-fun "fulfilled"))))))
        (completed-value))
    (is (eq :not-ready (fresult future)))
    (fcompleted future (value) (setf completed-value value))
    (is (eq t (assert-cond (lambda () (string= "fulfilled" completed-value)) 1)))))

(test mapping-futures--with-fut-macro
  "Tests mapping futures"
  (flet ((future-generator (x)
           (with-fut (+ x 1))))
    (let ((future (fmap (future-generator 0) (completed-value)
                    (fmap (future-generator completed-value) (completed-value)
                      (fmap (future-generator completed-value) (completed-value)
                        completed-value)))))
      (is-true (assert-cond (lambda ()
                              (= 3 (fresult future)))
                            1)))))

(test mapping-using-arrows
  "Tests fmap using arrows aka threading with
mixed future, normal and async-future map-fun result."
  (let ((completed-val))
    (-> (with-fut 0)
      (fmap (value)
          (+ value 1))
      (fmap (value)
          (with-fut-resolve
            (sleep 0.2)
            (fresolve (+ value 1))))
      (fmap (value)
          (+ value 1))
      (fcompleted (compl-value)
          (setf completed-val compl-value)))
    (is-true (assert-cond
              (lambda () (eq completed-val 3)) 1))))

(test mapping--fut-errors
  "Tests fmap but one future errors, catch it with `frecover'"
  (is (string= "foo"
               (fresult
                (frecover
                 (-> (with-fut 0)
                   (fmap (value)
                     (with-fut (+ value 1)))
                   (fmap (value)
                     (declare (ignore value))
                     (error "foo"))
                   (fmap (value)
                     (+ value 1)))
                 (error (c) (format nil "~a" c)))))))

(test mapping-with-fcompleted
  (let ((completed-val))
    (-> (with-fut 0)
      (fmap (value)
        (with-fut (+ value 1)))
      (fcompleted (value)
          (setf completed-val value)))
    (is-true (assert-cond (lambda ()
                            (= 1 completed-val))
                          1))))

(test await-fut
  (multiple-value-bind (res fut)
      (fawait (with-fut 0) :timeout 1)
    (is (= 0 res))
    (is (futurep fut))))
