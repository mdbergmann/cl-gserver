(defpackage :sento.miscutils
  (:nicknames :miscutils)
  (:use :cl)
  (:export #:mkstr
           #:assert-cond
           #:await-cond
           #:filter
           #:collect-backtrace))

(in-package :sento.miscutils)

(defun mkstr (&rest args)
  "Converts all parameters to string and concatenates them."
  (with-output-to-string (stream)
    (dolist (a args) (princ a stream))))

(defun filter (fun lst)
  (mapcan (lambda (x) (if (funcall fun x) (list x))) lst))

(defun collect-backtrace (condition)
  (let ((backtrace (make-string-output-stream)))
    (uiop:print-condition-backtrace condition :stream backtrace)
    (get-output-stream-string backtrace)))

(defun assert-cond (assert-fun max-time &optional (sleep-time 0.05))
  "Obsolete, use `await-cond' instead."
  (do ((wait-time sleep-time (+ wait-time sleep-time))
       (fun-result nil (funcall assert-fun)))
      ((not (null fun-result)) (return t))
    (if (> wait-time max-time) (return)
        (sleep sleep-time))))

(defmacro await-cond (max-time &body body)
  "Awaits condition. Probes repeatedly.
If after `max-time' condition is not `t' it is considered failed."
  `(assert-cond (lambda ()
                  ,@body)
                ,max-time))
