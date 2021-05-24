(defpackage :cl-gserver.tasks
  (:use :cl)
  (:nicknames :tasks)
  (:export #:with-context
           #:task-yield
           #:task-start)
  )

(in-package :cl-gserver.tasks)

(defvar *task-context*)

(defmacro with-context (context &body body)
  `(let ((*task-context* ,context))
     ,@body))

(defun make-tmp-actor (context)
  (act:actor-of (context)
    :receive (lambda (self msg state)
               (declare (ignore self))
               (cond
                 ((eq :exec (car msg))
                  (cons (funcall (cdr msg)) state))
                 (t (cons :unrecognized-command state))))))

(defun task-yield (fun &optional time-out)
  (let ((tmp-actor (make-tmp-actor *task-context*)))
    (unwind-protect
         (act:ask-s tmp-actor (cons :exec fun) :time-out time-out)
      (ac:stop *task-context* tmp-actor))))

(defun task-start (fun)
  (let ((tmp-actor (make-tmp-actor *task-context*)))
    (unwind-protect
         (progn
           (act:tell tmp-actor (cons :exec fun))
           (values :ok tmp-actor))
      (ac:stop *task-context* tmp-actor))))
