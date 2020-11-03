(defpackage :cl-gserver.utils
  (:nicknames :utils)
  (:use :cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:mkstr
           #:assert-cond
           #:filter
           #:wait-cond
           #:wait-expired
           #:with-waitfor
           #:cause))

(in-package :cl-gserver.utils)

(defun mkstr (&rest args)
  "Converts all parameters to string and concatenates them."
  (with-output-to-string (stream)
    (dolist (a args) (princ a stream))))

(defun assert-cond (assert-fun max-time)
  (do ((wait-time 0.02 (+ wait-time 0.02))
       (fun-result nil (funcall assert-fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep 0.02))))

(defmacro filter (fun list)
  (with-gensyms (x)
  `(mapcan (lambda (,x) (if (funcall ,fun ,x) (list ,x))) ,list)))

(defun wait-cond (cond-fun &optional (sleep-time 0.02) (max-time 12))
  (let ((wait-acc 0))
    (if (not (funcall cond-fun))
        (loop
           (sleep sleep-time)
           (+ sleep-time wait-acc)
           (when (or (funcall cond-fun) (> wait-acc max-time)) (return))))))

(define-condition wait-expired (serious-condition)
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
  (alexandria:with-gensyms (c)
    `(handler-case
         (bt:with-timeout (,wait-time)
                     ,@body)
       (bt::interrupt (,c)
         (log:warn "Interrupted, wrapping to 'expired'.")
         (error 'wait-expired :wait-time ,wait-time :cause ,c))
       (bt:timeout (,c)
         (log:warn "bt:timeout, wrapping to 'expired'.")
         (error 'wait-expired :wait-time ,wait-time :cause ,c))
       #+sbcl
       (sb-ext:timeout (,c)
         (log:warn "sb-ext:timeout, wrapping to 'expired'.")
         (error 'wait-expired :wait-time ,wait-time :cause ,c)))))
