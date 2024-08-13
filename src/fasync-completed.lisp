(defpackage :sento.async-future
  (:use :cl :future)
  (:nicknames :async-future)
  (:export #:fasync-completed))

(in-package :sento.async-future)

(defmacro fasync-completed (future context dispatcher-id (result) &body body)
  "Asynchronous future completion handler.

This work essentially the same as `future:fcompleted` except that the completion function executes in a different execution context.
The 'execution-context' is a dispatcher (`disp:dispatcher`) registered in `asys:actor-system`.
It is here identified using `dispatcher-id` (the defailt dispatcher identifier is `:shared`).
The additional parameter `context` can be the actor-system itself, an `ac:actor-context` or an `act:actor` instance.

If the completion handler should execute on the caller thread, then `future:fcompleted` should be used.

If the `future` is already complete then the `body` executes immediately.
`result` represents the future result.
`body` is executed when future completed.
Returns the future.

Example:

```
(fasync-completed (with-fut (sleep .5) 1) asys :shared
            (result)
  (format t \"Future result ~a~%\" result))
```
"
  (let ((disp (gensym "disp"))
        (sys (gensym "sys")))
    `(progn
       (assert (typep ,future 'future) nil "Arg 'future' is not of required type!")
       (assert (or (typep ,context 'asys:actor-system)
                   (typep ,context 'ac:actor-context)
                   (typep ,context 'act:actor))
               nil "Arg 'context' is not of required type!")
       (let* ((,sys (typecase ,context
                      (asys:actor-system ,context)
                      (otherwise (ac:system ,context))))
              (,disp (getf (asys:dispatchers ,sys) ,dispatcher-id)))
         (assert (not (null ,disp)) nil "Dispatcher-id is not known!")
         (disp:dispatch-async
          ,disp
          (list 
           (lambda ()
             (future::%fcompleted ,future (lambda (,result) ,@body))))))
       ,future)))

