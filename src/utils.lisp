(defpackage :cl-gserver.utils
  (:nicknames :utils)
  (:use :cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:mkstr
           #:assert-cond
           #:filter
           #:wait-cond
           #:ask-timeout
           #:with-waitfor
           #:cause
           #:make-timer
           #:collect-backtrace))

(in-package :cl-gserver.utils)

(defun mkstr (&rest args)
  "Converts all parameters to string and concatenates them."
  (with-output-to-string (stream)
    (dolist (a args) (princ a stream))))

(defun assert-cond (assert-fun max-time &optional (sleep-time 0.05))
  (do ((wait-time sleep-time (+ wait-time sleep-time))
       (fun-result nil (funcall assert-fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep sleep-time))))

(defun filter (fun lst)
  (mapcan (lambda (x) (if (funcall fun x) (list x))) lst))

(defun wait-cond (cond-fun &optional (sleep-time 0.02) (max-time 12))
  (let ((wait-acc 0))
    (if (not (funcall cond-fun))
        (loop
           (sleep sleep-time)
           (+ sleep-time wait-acc)
           (when (or (funcall cond-fun) (> wait-acc max-time)) (return))))))

(define-condition ask-timeout (serious-condition)
  ((wait-time :initform nil
              :initarg :wait-time
              :reader wait-time)
   (cause :initform nil
          :initarg :cause
          :reader cause))
  (:report (lambda (c stream)
             (format stream "A timeout set to ~a seconds occurred. Cause: "
                     (wait-time c))
             (print (cause c) stream))))

(defmacro with-waitfor ((wait-time) &body body)
  "Spawns thread with timeout. Blocks until computation is done, or timeout elapsed."
  (with-gensyms (c)
    `(handler-case
         (bt:with-timeout (,wait-time)
                     ,@body)
       (bt:timeout (,c)
         (error ,c))
       ;; the below is not needed anymore with SBCL 2.1. Will keep it anyway for compatibility.
       #+sbcl
       (sb-ext:timeout (,c)
         (log:warn "sb-ext:timeout, wrapping to 'expired'.")
         (error 'bt:timeout :length ,wait-time)))))

(defun make-timer (delay run-fun)
  (bt:make-thread (lambda ()
                    (sleep delay)
                    (funcall run-fun))
                  :name (string (gensym "timer-"))))

(defun collect-backtrace (condition)
  (let ((backtrace (make-string-output-stream)))
    (uiop:print-condition-backtrace condition :stream backtrace)
    (get-output-stream-string backtrace)))
