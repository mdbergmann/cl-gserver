(defpackage :cl-gserver.future
  (:use :cl :blackbird)
  (:nicknames :future)
  (:export #:future
           #:make-future
           #:complete-p
           #:on-completed
           #:get-result))

(in-package :cl-gserver.future)

(defclass future ()
  ((promise :initform nil))
  (:documentation
   "The wrapped [blackbird](https://orthecreedence.github.io/blackbird/) `promise`, here called `future`.  
Not all features of blackbird's `promise` are supported.  
This `future` wrapper changes the terminology. A `future` is a delayed computation.
A `promise` is the fulfillment of the delayed computation.

The `future` is used as part of `act:ask` but is available as a general utility."))

(defmethod print-object ((obj future) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (promise) obj
      (format stream "promise: ~a" promise))))

(defun make-future (execute-fun)
  "Creates a future. `execute-fun` is the lambda that is executed when the future is created.
`execute-fun` takes a parameter which is the `resolve-fun` funtion. `resolve-fun` function
takes the `promise` as parameter which is the computed value. Calling `resolve-fun` with the promise
will fulfill the `future`.  
Manually calling `resolve-fun` to fulfill the `future` is in contrast to just fulfill the `future` from a return value. The benefit of the `resolve-fun` is flexibility. In  a multi-threaded environment `execute-fun` could spawn a thread, in which case `execute-fun` would return immediately but no promise can be given at that time. The `resolve-fun` can be called from a thread and provide the promise.

Create a future with:

```elisp
(make-future (lambda (resolve-fun) 
               (let ((promise (delayed-computation)))
                 (bt:make-thread (lambda ()
                   (sleep 0.5)
                   (funcall resolve-fun promise))))))
```
"
  (let ((future (make-instance 'future)))
    (with-slots (promise) future
      (setf promise
            (create-promise (lambda (resolve-fn reject-fn)
                              (declare (ignore reject-fn))
                              (funcall execute-fun resolve-fn)))))
    future))

(defun complete-p (future)
  "Is `future` completed? Returns either `t` or `nil`."
  (with-slots (promise) future
    (promise-finished-p promise)))

(defun on-completed (future completed-fun)
  "Install an on-completion handler function on the given `future`.
If the `future` is already complete then the `completed-fun` function is called immediately.
`completed-fun` takes a parameter which represents the fulfilled promise (the value with which the `future` was fulfilled)."
  (with-slots (promise) future
    (attach promise completed-fun))
  nil)

(defun get-result (future)
  "Get the computation result. If not yet available `:not-ready` is returned."
  (with-slots (promise) future
    (with-slots (values) promise
      (if (null values)
          :not-ready
          (car values)))))
