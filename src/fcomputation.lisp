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
  ((the-promise :initform nil
                :documentation "The wrapped promise")))

(defun make-future (execute-fun)
  (let ((future (make-instance 'future)))
    (with-slots (the-promise) future
      (setf the-promise
            (create-promise (lambda (resolve-fn reject-fn)
                              (declare (ignore reject-fn))
                              (funcall execute-fun resolve-fn)))))
    future))

(defun complete-p (future)
  "Is `future' completed? Returns either `t' or `nil'."
  (with-slots (the-promise) future
    (promise-finished-p the-promise)))

(defun on-completed (future completed-fun)
  "Install an on-completion handler function on the given `future'.
If the `future' is already complete then the `completed-fun' function is called immediately."
  (with-slots (the-promise) future
    (attach the-promise completed-fun)))

(defun get-result (future)
  "Get the computation result. If not yet available `:not-ready' is returned."
  (with-slots (the-promise) future
    (with-slots (values) the-promise
      (if (null values)
          :not-ready
          (car values)))))
