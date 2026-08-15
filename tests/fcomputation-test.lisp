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

(test complete-with-error
  "Test the completion with fcompleted callback with an error."

  (let ((future (make-future (lambda (resolve-fun)
                               (declare (ignore resolve-fun))
                               (error "Some error")))))
    (is (complete-p future))
    (is (error-p future))
    (is (typep (fresult future) 'simple-error))
    (is (equal "Some error" (simple-condition-format-control (fresult future))))))

(test mapping-futures--with-fut-macro
  "Tests mapping futures"
  (flet ((future-generator (x)
           (with-fut (+ x 1))))
    (let ((future (fmap (future-generator 0) (completed-value)
                    (fmap (future-generator completed-value) (completed-value)
                      (fmap (future-generator completed-value) (completed-value)
                        completed-value)))))
      (is-true (assert-cond (lambda ()
                              (eql 3 (fresult future)))
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

(test fresult--completed-with-nil
  "A future completed with NIL is distinguishable from a pending future."
  (let ((future (with-fut nil)))
    (is-true (complete-p future))
    (is (null (fresult future)))))

(test fresult--completed-with-zero-values
  "A future resolved without any values reports completion with a NIL result."
  (let ((future (make-future (lambda (resolve-fun)
                               (funcall resolve-fun)))))
    (is-true (complete-p future))
    (is (null (fresult future)))))

(test fawait--completed-with-nil--returns-immediately
  "fawait must not poll until timeout when the future completed with NIL."
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (res fut)
        (fawait (with-fut nil) :timeout 2)
      (is (null res))
      (is-true (complete-p fut)))
    (is (> 1.0 (/ (- (get-internal-real-time) start)
                  internal-time-units-per-second)))))

(test fcompleted--completed-with-nil
  "fcompleted fires for a future completed with NIL."
  (let ((completed nil))
    (fcompleted (with-fut nil) (result)
      (setf completed (list :done result)))
    (is (equal '(:done nil) completed))))

(test fcompleted--handler-error--does-not-affect-other-handlers
  "An error in one completion handler must not prevent other handlers from running."
  (let ((completed nil)
        (future (with-fut-resolve
                  (bt2:make-thread (lambda ()
                                     (sleep 0.2)
                                     (fresolve 1))))))
    (fcompleted future (result)
      (declare (ignore result))
      (error "handler error"))
    (fcompleted future (result)
      (setf completed result))
    (is-true (assert-cond (lambda () (eql 1 completed)) 1))))

(test fcompleted--concurrent-attach-and-resolve
  "Stress test: no completion may be lost when the future is resolved from
another thread while the completion handler is being attached."
  (let ((lost 0))
    (loop :repeat 200
          :do (let* ((completed nil)
                     (future (make-future
                              (lambda (resolve-fun)
                                (bt2:make-thread
                                 (lambda ()
                                   (funcall resolve-fun 42)))))))
                (fcompleted future (result)
                  (declare (ignore result))
                  (setf completed t))
                (unless (assert-cond (lambda () completed) 0.5)
                  (incf lost))))
    (is (= 0 lost))))

(test fmap--completed-with-nil
  "fmap maps a future that completed with NIL."
  (let ((future (fmap (with-fut nil) (result)
                  (list :mapped result))))
    (is-true (assert-cond (lambda ()
                            (equal '(:mapped nil) (fresult future)))
                          1))))

(test frecover--unmatched-condition--propagates
  "A condition not matched by any frecover handler propagates to the result future."
  (let ((future (frecover (with-fut (error 'type-error :datum 1 :expected-type 'string))
                          (simple-error (c) (declare (ignore c)) :recovered))))
    (is-true (error-p future))
    (is (typep (fresult future) 'type-error))))
