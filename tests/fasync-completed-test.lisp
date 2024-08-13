(defpackage :sento.async-future-test
  (:use :cl :fiveam :sento.future :sento.async-future :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.async-future-test)

(def-suite async-future-tests
  :description "Tests for async future completion."
  :in sento.tests:test-suite)

(in-suite async-future-tests)

(def-fixture with-sys ()
  (let ((asys (asys:make-actor-system)))
    (unwind-protect
         (progn (&body))
      (ac:shutdown asys))))      

(test future-completion-works-async
  "Async as in it's executed by a defined dispatcher."
  (with-fixture with-sys ()
    (let ((completed-result nil)
          (fut (with-fut-resolve
                 (Format t "~%Called on: ~a~%" (bt2:current-thread))
                 (fresolve (+ 1 2)))))
      (fasync-completed fut asys :shared
          (result)
        (format t "~%Completed in: ~a~%" (bt2:current-thread))
        (setf completed-result result))
      (is-true (await-cond 0.5
                 (eql completed-result 3))))))

(test future-completion-async--error--no-future
  (with-fixture with-sys ()
    (handler-case
        (fasync-completed nil nil :shared
            (result)
          (declare (ignore result)))
      (error (c)
        (is (string= "Arg 'future' is not of required type!"
                     (simple-condition-format-control c))))
      (:no-error () (fail "Should not be here!")))))

(test future-completion-async--error--no-context
  (with-fixture with-sys ()
    (handler-case
        (fasync-completed (with-fut "foo") nil :shared
            (result)
          (declare (ignore result)))
      (error (c)
        (is (string= "Arg 'context' is not of required type!"
                     (simple-condition-format-control c))))
      (:no-error () (fail "Should not be here!")))))

(test future-completion-async--error--dispatcher-not-known
  (with-fixture with-sys ()
    (handler-case
        (fasync-completed (with-fut "foo") asys :not-exists
            (result)
          (declare (ignore result)))
      (error (c)
        (is (string= "Dispatcher-id is not known!"
                     (simple-condition-format-control c))))
      (:no-error () (fail "Should not be here!")))))

(test future-completion-async--allow-more-context-types
  (with-fixture with-sys ()
    (let ((act (ac:actor-of asys :receive (lambda (msg) msg)))
          (completed-result nil)
          (fut (with-fut (+ 1 2))))
      (fasync-completed fut act :shared
          (result)
        (format t "~%Completed in: ~a~%" (bt2:current-thread))
        (setf completed-result result))
      (is-true (await-cond 0.5
                 (eql completed-result 3))))))
