(defpackage :cl-gserver.tasks
  (:use :cl)
  (:nicknames :tasks)
  (:export #:with-context
           #:task-yield
           #:task-start
           #:task-async
           #:task-await)
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
                 ((eq :get msg)
                  (cons state state))
                 ((eq :exec (car msg))
                  (let ((fun-result (funcall (cdr msg))))
                    (cons fun-result fun-result)))
                 (t (cons :unrecognized-command state))))))

(defun task-yield (fun &optional time-out)
  "`task-yield` runs the given function `fun` in a temporary spawned actor by blocking and waiting for a response from the actor, or until the given timeout was elapsed.

A normal response from the actor is passed back as the response value.
If the timeout elapsed the response is: `(cons :handler-error utils:ask-timeout)`."
  (let ((tmp-actor (make-tmp-actor *task-context*)))
    (prog1
        (act:ask-s tmp-actor (cons :exec fun) :time-out time-out)
      (ac:stop *task-context* tmp-actor))))

(defun task-start (fun)
  "`task-start` runs the given function `fun` asynchronously in a temporary actor.
Use this if you don't care about any response or result, i.e. for I/O side-effects.
It returns `(values :ok <actor-ref>)`. `<actor-ref> is the actor given back as reference.
The temporary actor is automatically stopped and removed from the context and will not be able to handle requests."
  (let ((tmp-actor (make-tmp-actor *task-context*)))
    (unwind-protect
         (progn
           (act:tell tmp-actor (cons :exec fun))
           (values :ok tmp-actor))
      (ac:stop *task-context* tmp-actor))))

(defun task-async (fun)
  "`task-async` schedules the function `fun` for asynchronous execution in a temporary spanwed actor.
The result of this function is the temporary spawned actor as 'task'.
Store this result for a call to `task-async`."
  (let ((tmp-actor (make-tmp-actor *task-context*)))
    (act:tell tmp-actor (cons :exec fun))
    tmp-actor))

(defun task-await (task-actor)
  "`task-await` waits (by blocking) until a result has been generated for a previous `task-async` by passing the 'task' result of `task-async` (which is effectively an actor instance) to `task-await`.
`task-await` also stops the temporary actor that is the result of `task-async`, so it is of no further use."
  (prog1
    (act:ask-s task-actor :get)
    (ac:stop *task-context* task-actor)))
