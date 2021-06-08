(defpackage :cl-gserver.tasks
  (:use :cl)
  (:nicknames :tasks)
  (:import-from #:binding-arrows
                #:->>)
  (:export #:with-context
           #:task
           #:task-yield
           #:task-start
           #:task-async
           #:task-await
           #:task-shutdown
           #:task-async-stream))

(in-package :cl-gserver.tasks)

(defvar *task-context*)
(defvar *task-dispatcher*)

(defmacro with-context ((context &optional (dispatcher :shared)) &body body)
  "`with-context` creates an environment where the `tasks` package functions should be used in.
`context` can be either an `asys:actor-system`, an `ac:actor-context`, or an `act:actor` (or subclass).
`dispatcher` specifies the dispatcher where the tasks is executed in (like thread-pool).
The tasks created using the `tasks` functions will then be created in the given context.

Example:

```elisp
;; create actor-system
(defparameter *sys* (make-actor-system))

(with-context (*sys*)
  (task-yield (lambda () (+ 1 1))))

=> 2 (2 bits, #x2, #o2, #b10)

```

Since the default `:shared` dispatcher should mainly be used for the message dispatching, 
but not so much for longer running tasks it is possible to create an actor system with additional
dispatchers. This additional dispatcher can be utilized for `tasks`. Be aware that the config as used below is merged with the `asys:*default-config*` which means that the dispatcher `:foo` here is really an additional dispatcher.

```
;; create actor-system with additional (custom) dispatcher
(defparameter *sys* (asys:make-actor-system '(:dispatchers (:foo (:workers 16)))))

(with-context (*sys* :foo)
  (task-yield (lambda () (+ 1 1))))

```
"
  `(let ((*task-context* ,context)
         (*task-dispatcher* ,dispatcher))
     ,@body))

(defclass task (act:actor) ()
  (:documentation
   "A dedicated `act:actor` subclass used for tasks."))

(defun make-task (context dispatcher)
  (act:actor-of (context)
    :dispatcher dispatcher
    :type 'task
    :receive (lambda (self msg state)
               (declare (ignore self))
               (cond
                 ((eq :get msg)
                  (cons state state))
                 ((eq :exec (car msg))
                  (handler-case
                      (let ((fun-result (funcall (cdr msg))))
                        (cons fun-result fun-result))
                    (error (c)
                      (let ((err-result (cons :handler-error c)))
                        (cons err-result err-result)))))
                 (t (cons :unrecognized-command state))))))

(defun task-yield (fun &optional time-out)
  "`task-yield` runs the given function `fun` by blocking and waiting for a response from the `task`, or until the given timeout was elapsed.
`fun` must be a 0-arity function.

A normal response from the actor is passed back as the response value.
If the timeout elapsed the response is: `(values :handler-error utils:ask-timeout)`.

Example:

```elisp
;; create actor-system
(defparameter *sys* (make-actor-system))

(with-context (*sys*)
  (task-yield (lambda () (+ 1 1))))

=> 2 (2 bits, #x2, #o2, #b10)
```
"
  (let ((task (make-task *task-context* *task-dispatcher*)))
    (unwind-protect
         (let ((ask-result (act:ask-s task (cons :exec fun) :time-out time-out)))
           (cond
             ((consp ask-result)
              (values (car ask-result) (cdr ask-result)))
             (t ask-result)))
      (ac:stop *task-context* task))))

(defun task-start (fun)
  "`task-start` runs the given function `fun` asynchronously.
`fun` must be a 0-arity function.
Use this if you don't care about any response or result, i.e. for I/O side-effects.
It returns `(values :ok <task>)`. `<task> is in fact an actor given back as reference.
The task is automatically stopped and removed from the context and will not be able to handle requests."
  (let ((task (make-task *task-context* *task-dispatcher*)))
    (unwind-protect
         (progn
           (act:tell task (cons :exec fun))
           (values :ok task))
      (ac:stop *task-context* task))))

(defun task-async (fun &key on-complete-fun)
  "`task-async` schedules the function `fun` for asynchronous execution.
`fun` must be a 0-arity function.
`on-complete-fun` is a 1-arity completion handler function. When called the result is delivered.
The completion handler function parameter may also be a `(cons :handler-error condition)` construct in case an error happened within the message handling.

Using `task-async` provides two alternatives:

- together with `task-await`
- or with completion handler

In fact it is possible to call `task-await` as well, but then you probably don't need a completion handler.
Using the completion handler makes the processing complete asynchronous.

The result of `task-async` is a `task`.
Store this `task` for a call to `task-async` (even with or without using `on-complete-fun`).
When _not_ using `on-complete-fun` users must call either `task-await` or `task-shutdown` for the task to be cleaned up.
When using `on-complete-fun` this is done for you.

Example:

```elisp
;; create actor-system
(defparameter *sys* (make-actor-system))

(with-context (*sys*)
  (let ((x (task-async (lambda () (some bigger computation))))
        (y 1))
    (+ (task-await x) y)))

;; use-case with `on-complete-fun`
(defun my-task-completion (result)
  (do-something-with result))

(with-context (*sys*)
  (task-async (lambda () (some-bigger-computation))
              :on-complete-fun #'my-task-completion))
```
"
  (let ((task (make-task *task-context* *task-dispatcher*)))
    (if on-complete-fun
        (progn
          (future:on-completed (act:ask task (cons :exec fun))
                               (lambda (result)
                                 (funcall on-complete-fun result)
                                 (act:tell task :stop)))
          task)
        (progn 
          (act:tell task (cons :exec fun))
          task))))

(defun task-await (task &optional time-out)
  "`task-await` waits (by blocking) until a result has been generated for a previous `task-async` by passing the `task` result of `task-async` to `task-await`.
Specify `time-out` in seconds. If `task-await` times out a `(cons :handler-error 'ask-timeout)` will be returned.
`task-await` also stops the `task` that is the result of `task-async`, so it is of no further use."
  (let ((task-state (act-cell:state task)))
    (if task-state
        task-state
        (unwind-protect
             (act:ask-s task :get :time-out time-out)
          (ac:stop *task-context* task)))))

(defun task-shutdown (task)
  "`task-shutdown` shuts down a task in order to clean up resources."
  (ac:stop *task-context* task))

(defun task-async-stream (fun lst)
  "`task-async-stream` concurrently applies `fun` on all elements of `lst`.
`fun` must be a one-arity function taking an element of `lst`.

The concurrency depends on the number of available `:shared` dispatcher workers.
Each element of `lst` is processed by a worker of the `asys:actor-system`s `:shared` dispatcher.
If all workers are busy then the computation of `fun` is queued.

Example:

```elisp
;; create actor-system
(defparameter *sys* (make-actor-system))

(with-context (*sys*)
  (->> 
    '(1 2 3 4 5)
    (task-async-stream #'1+)
    (reduce #'+)))

=> 20 (5 bits, #x14, #o24, #b10100)
```
"
  (->>
    lst
    (mapcar (lambda (x) (task-async (lambda () (funcall fun x)))))
    (mapcar #'task-await)))
