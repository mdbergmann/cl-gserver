(defpackage :cl-gserver.tasks
  (:use :cl)
  (:nicknames :tasks)
  (:export #:with-context
           #:task-yield)
  )

(in-package :cl-gserver.tasks)

(defvar *task-context*)

(defmacro with-context (context &body body)
  `(let ((*task-context* ,context))
     ,@body))

(defun task-yield (fun &optional time-out)
  (let ((tmp-actor (act:actor-of (*task-context*)
                     :receive (lambda (self msg state)
                                (declare (ignore self))
                                (cond
                                  ((eq :exec (car msg))
                                   (cons (funcall (cdr msg)) state))
                                  (t (cons :unrecognized state)))))))
    (unwind-protect
         (act:ask-s tmp-actor (cons :exec fun) :time-out time-out)
      (ac:stop *task-context* tmp-actor))))

