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
  "`task-yield` runs the given function `fun` in a temporary spawned actor by blocking and waiting for a response from the actor, or until the given timeout was elapsed.

A normal response from the actor is passed back as the response value.
If the timeout elapsed the response is: `(cons :handler-error utils:ask-timeout)`."
  (let ((tmp-actor (make-tmp-actor *task-context*)))
    (unwind-protect
         (act:ask-s tmp-actor (cons :exec fun) :time-out time-out)
      (ac:stop *task-context* tmp-actor))))

(defun task-start (fun)
  "`task-start` runs the given function `fun` asynchronously in a temporary actor.
It returns `(values :ok <actor-ref>)`. `<actor-ref> is the actor given back as reference.
The temporary actor is automatically stopped and removed from the context and will not be able to handle requests."
  (let ((tmp-actor (make-tmp-actor *task-context*)))
    (unwind-protect
         (progn
           (act:tell tmp-actor (cons :exec fun))
           (values :ok tmp-actor))
      (ac:stop *task-context* tmp-actor))))
