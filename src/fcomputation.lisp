(defpackage :cl-gserver.fcomputation
  (:use :cl)
  (:nicknames :fcomputation)
  (:export #:fcomputation
           #:complete-p
           #:on-completed
           #:get-result))

(in-package :cl-gserver.fcomputation)

(defclass fcomputation ()
  ((complete-p-fun :initform nil
                   :documentation
                   "Check if the computation is complete. Should return `t' or `nil'")
   (get-result-fun :initform nil
                   :documentation
                   "The function that returns the computed value. If the value is not computed yet it returns `:not-ready'")
   (exec-fun :initarg :exec-fun
             :initform (error "Must be initialized with function!")
             :documentation
             "The asynchronous function to be executed. It will be executed as part of the `fcomputation'.
The `exec-fun' function (or lambda) takes one argument.
This argument is a `ready'-function that must be called when `exec-fun' is donr with the computation.
If there was a value computed, then `ready-fun' must be called with this computed value or nil.")
   (on-completed-fun :initarg :on-completed-fun
                     :initform nil
                     :documentation
                     "On completion function handler. It is called with the computed value as argument."))
  (:documentation
   "This class represents a future computation. 
It is primarily meant to be used in conjunction with a Gserver or subclasses thereof."))

(defmethod initialize-instance :after ((self fcomputation) &key)
  (with-slots (exec-fun on-completed-fun get-result-fun complete-p-fun) self
    (let* ((async-result :not-ready)
           (set-computation-value-fun (lambda (computation-value)
                                        (log:debug "Setting computed value to: " computation-value)
                                        (setf async-result computation-value)
                                        (when on-completed-fun
                                          (funcall on-completed-fun computation-value)))))
      (setf get-result-fun  (lambda () async-result))
      (setf complete-p-fun (lambda () (not (eq :not-ready async-result))))
    
      (funcall exec-fun set-computation-value-fun))))

(defun complete-p (fcomputation)
  "Is `fcomputation' completed? Returns either `t' or `nil'."
  (funcall (slot-value fcomputation 'complete-p-fun)))

(defun on-completed (fcomputation completed-fun)
  "Install an on-completion handler function on the given `fcomputation'.
If the `fcomputation' is already complete then the `completed-fun' function is called immediately."
  (with-slots (get-result-fun complete-p-fun on-completed-fun) fcomputation
    ;; install on-completed fun
    (unless on-completed-fun
      (setf on-completed-fun completed-fun))
    ;; call immediately when completed
    (when (funcall complete-p-fun)
      (funcall on-completed-fun (funcall get-result-fun)))))

(defun get-result (fcomputation)
  "Get the computation result. If not yet available `:not-ready' is returned."
  (funcall (slot-value fcomputation 'get-result-fun)))
