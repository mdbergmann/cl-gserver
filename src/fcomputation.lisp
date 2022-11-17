(defpackage :sento.future
  (:use :cl :blackbird)
  (:nicknames :future)
  (:export #:future
           #:with-fut
           #:with-fut-resolve
           #:make-future
           #:futurep
           #:complete-p
           #:fcompleted
           #:fresult
           #:frecover
           #:fmap
           #:fresolve))

(in-package :sento.future)

(defclass future ()
  ((promise :initform nil))
  (:documentation
   "The wrapped [blackbird](https://orthecreedence.github.io/blackbird/) `promise`, here called `future`.  
Not all features of blackbird's `promise` are supported.  
This `future` wrapper changes the terminology. A `future` is a delayed computation.
A `promise` is the fulfillment of the delayed computation."))

(defmethod print-object ((obj future) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (promise) obj
      (format stream "promise: ~a" promise))))

(defmacro with-fut (&body body)
  "Convenience macro for creating a `future`.

The `future` will be resolved with the result of the body form."
  `(make-future (lambda (resolve-fun)
                  (let ((result (progn ,@body)))
                    (funcall resolve-fun result)))))

(defmacro with-fut-resolve (&body body)
  "Convenience macro for creating a `future` that must be resolved manually via `resolve`.

This allows to spawn threads or other asynchronous operations as part of `body`.
However, you have to `resolve` the future eventually by applying a result on `resolve`.

Example:

```
(with-fut-resolve
  (bt:make-thread
   (lambda ()
     (fresolve (do-some-lengthy-calculation)))))
```
"
  `(macrolet ((fresolve (resolve-form)
                `(make-future (lambda (resolve-fun)
                                (let ((resolved ,resolve-form))
                                  (funcall resolve-fun resolved))))))
     ,@body))

(defun make-future (execute-fun)
  "Creates a future. `execute-fun` is the lambda that is executed when the future is created.
`execute-fun` takes a parameter which is the `execute-fun` funtion. `execute-fun` function
takes the `promise` as parameter which is the computed value. Calling `execute-fun` with the promise
will fulfill the `future`.  
Manually calling `execute-fun` to fulfill the `future` is in contrast to just fulfill the `future` from a return value. The benefit of the `execute-fun` is flexibility. In  a multi-threaded environment `execute-fun` could spawn a thread, in which case `execute-fun` would return immediately but no promise-value can be given at that time. The `execute-fun` can be called from a thread and provide the promise.

Create a future with:

```elisp
(make-future (lambda (execute-fun) 
               (let ((promise (delayed-computation)))
                 (bt:make-thread (lambda ()
                   (sleep 0.5)
                   (funcall execute-fun promise))))))
```
"
  (let ((future (make-instance 'future)))
    (with-slots (promise) future
      (setf promise
            (create-promise (lambda (resolve-fn reject-fn)
                              (declare (ignore reject-fn))
                              (funcall execute-fun resolve-fn)))))
    future))

(defun make-future-plain (p)
  (let ((future (make-instance 'future)))
    (with-slots (promise) future
      (setf promise p))
    future))

(defun futurep (object)
  "Checks if type of `object` is `future`."
  (typep object 'future))

(defun complete-p (future)
  "Is `future` completed? Returns either `t` or `nil`."
  (with-slots (promise) future
    (promise-finished-p promise)))

(defun fcompleted (future completed-fun)
  "Install an completion handler function on the given `future`.
If the `future` is already complete then the `completed-fun` function is called immediately.
`completed-fun` takes a parameter which represents the fulfilled promise (the value with which the `future` was fulfilled)."
  (with-slots (promise) future
    (attach promise completed-fun))
  nil)

(defun fresult (future)
  "Get the computation result. If not yet available `:not-ready` is returned."
  (with-slots (promise) future
    (let ((the-promise (blackbird-base::lookup-forwarded-promise promise)))
      (with-slots (values) the-promise
        (if (null values)
            :not-ready
            (car values))))))

(defmacro frecover (future &rest handler-forms)
  "Catch errors in futures using `frecover`
It works similar to `handler-case`.

Example:

```
(fresult
 (frecover
  (-> (with-fut 0)
    (fmap (value)
            (with-fut (+ value 1))))
    (fmap (value)
            (declare (ignore value))
            (error \"foo\")))
    (fmap (value)
            (+ value 1))))
  (error (c) (format nil \"~a\" c))))
```
"
  `(with-slots (promise) ,future
     (make-future-plain (catcher promise ,@handler-forms))))

(defun %fmap (future map-fun)
  (with-slots (promise) future
    (let* ((the-promise (blackbird-base::lookup-forwarded-promise promise))
           (cb-return-promise (blackbird-base::make-promise :name nil))
           (cb-wrapped (lambda (&rest args)
                         (blackbird-base::with-error-handling
                             (errexit promise)
                             (lambda (e)
                               (blackbird-base::signal-error cb-return-promise e)
                               (return-from errexit))
                           (let ((cb-return (multiple-value-list (apply map-fun args))))
                             ;; (format t "cb-return: ~a~%" cb-return)
                             ;; the below is a special treatment to make this
                             ;; work with out 'future'
                             ;; (format t "cb-return: ~a~%" cb-return)
                             (let ((new-cb-return
                                     (cond
                                       ((typep (car cb-return) 'future)
                                        (list (slot-value (car cb-return) 'promise)))
                                       (t
                                        cb-return))))
                               ;;(format t "new-cb-return: ~a~%" new-cb-return)
                               ;; (when (promisep (car new-cb-return))
                               ;;   (format t "promise values: ~a~%"
                               ;;           (blackbird-base::promise-values (car new-cb-return))))
                               (apply #'blackbird-base::finish
                                      (append
                                       (list cb-return-promise)
                                       new-cb-return))))))))
      (blackbird-base::attach-errback the-promise
                                      (lambda (e)
                                        (blackbird-base::signal-error
                                         cb-return-promise
                                         e)))
      (blackbird-base::do-add-callback
        the-promise
        (blackbird-base::wrap-callback cb-wrapped))
      (blackbird-base::run-promise the-promise)
      (make-future-plain cb-return-promise))))

(defmacro fmap (future (result) &body body)
  "`fmap` maps a future.

`future` is the future that is mapped.
`result` is the result of the future when it completed."
  `(%fmap ,future (lambda (,result) ,@body)))
