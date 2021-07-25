(defpackage :cl-gserver.agent.array
  (:use :cl)
  (:nicknames :agtarray)
  (:export #:make-array-agent
           #:agent-elt
           #:agent-push
           #:agent-push-and-getidx
           #:agent-pop
           #:agent-delete))

(in-package :cl-gserver.agent.array)

(defstruct model
  (arr nil :type array)
  (err-fun nil))

(defun make-array-agent (context &key
                                   initial-array
                                   (error-fun nil)
                                   (dispatcher-id :shared))
  "Creates an agent that wraps a CL array/vector.

`context`: something implementing `ac:actor-context` protocol like `asys:actor-system`. Specifying `nil` here creates an agent outside of an actor system. The user has to take care of that himself.  
`initial-array`: specify an initial array/vector.  
`error-fun`: a 1-arrity function taking a condition that was raised.
Use this to get notified of error when using the non-value returning functions of the agent.  
`dispatcher-id`: a dispatcher. defaults to `:shared`."
  (check-type initial-array array)
  (agt:make-agent (lambda () (make-model :arr initial-array
                                    :err-fun error-fun))
                  context dispatcher-id))

(defun agent-elt (index array-agent)
  "Retrieves the value of the specified index of the array. `agent-elt` allows `setf`ing like:

```
(setf (agent-elt 0 cut) 11)
```

`index`: the index to retrieve.  
`array-agent`: the array agent instance.

In case of error `agent-elt` returns the error condition that `elt` raises.

The `setf` functionality will call `err-fun` on error if it has been configured."
  (agt:agent-get array-agent
                 (lambda (model)
                   (handler-case
                       (elt (model-arr model) index)
                     (error (c) c)))))

(defmacro with-error-fun (&body body)
  `(lambda (model)
     (handler-case
         ,@body
       (error (c)
         (when (model-err-fun model)
           (funcall (model-err-fun model) c))))
     model))

(defmacro with-handler (&body body)
  `(lambda (model)
    (handler-case
        ,@body
      (error (c) c))))

(defun agent-set (index array-agent value)
  "Internal for `setf`."
  (agt:agent-update array-agent
                    (with-error-fun
                      (setf (elt (model-arr model) index) value)))
  value)

(defsetf agent-elt agent-set)

(defun agent-push (item array-agent)
  "Pushes a value to the array/vector. Internally uses `vector-push-extend`, so the array must have a `fill-pointer`.

`item`: item to push.  
`array-agent`: the array agent instance.

On error it will call `err-fun` with the raised condition, if `err-fun` has been configured."
  (agt:agent-update array-agent
                    (with-error-fun
                      (vector-push-extend item (model-arr model)))))

(defun agent-push-and-getidx (item array-agent)
  "Pushes `item` to the array. This function is similar to `agent-push` but returns the index of the pushed value similar as `vector-push` does. Therefore it is based on the somewhat slower `ask-s` actor pattern. So if you don't care about the new index of the pushed item use `agent-push` instead. But this one is able to immediately return error conditions that may occur on `vector-push`.

`item`: item to push.  
`array-agent`: the array agent instance."
  (agt:agent-get array-agent
                 (with-handler
                   (vector-push-extend item (model-arr model)))))

(defun agent-pop (array-agent)
  "Pops from array and returns the popped value. Internally uses `vector-pop`, so the array must have a `fill-pointer`. In case of error from using `vector-pop` the condition is returned.

`array-agent`: the array agent instance."
  (agt:agent-get array-agent
                 (with-handler
                       (vector-pop (model-arr model)))))

(defun agent-delete (item array-agent &rest delete-args)
  "Deletes item from array. Internally uses `delete`. Returns `T`.

`item`: the item to delete.  
`array-agent`: the array agent instance.  
`delete-args`: any arguments passed on to `delete`."
  (agt:agent-update array-agent
                    (with-error-fun
                      (let ((del-result (apply #'delete item (model-arr model) delete-args)))
                        del-result))))
