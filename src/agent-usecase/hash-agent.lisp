(defpackage :cl-gserver.agent.hash
  (:use :cl :cl-gserver.agent.usecase-commons)
  (:nicknames :agthash)
  (:export #:make-hash-agent
           #:agent-gethash
           #:agent-remhash
           #:agent-clrhash
           #:agent-dohash))

(in-package :cl-gserver.agent.hash)


(defun make-hash-agent (context &key
                                  initial-hash-table
                                  (error-fun nil)
                                  (dispatcher-id :shared))
  "Creates an agent that wraps a CL hash-table.

`context`: something implementing `ac:actor-context` protocol like `asys:actor-system`. Specifying `nil` here creates an agent outside of an actor system. The user has to take care of that himself.  
`initial-hash-table`: specify an initial hash-table.  
`error-fun`: a 1-arrity function taking a condition that was raised.
Use this to get notified of error when using the update functions of the agent.  
`dispatcher-id`: a dispatcher. defaults to `:shared`."
  (check-type initial-hash-table hash-table)
  (agt:make-agent (lambda ()
                    (make-model :object initial-hash-table
                                :err-fun error-fun))
                  context dispatcher-id))

(defun agent-puthash (key hash-agent value)
  "Internal"
  (agt:agent-update hash-agent
                    (with-update-handler
                      (setf (gethash key (model-object model)) value)))
  value)

(defun agent-gethash (key hash-agent)
  "Retrieves value from hash-table, or `nil` if it doesn't exist.
See `cl:gethash` for more info.

This supports setting a hash using `setf` in the same way as with `cl:hash-table`.

Returns any raised condition or the value from `gethash`."
  (agt:agent-get hash-agent
                 (with-get-handler
                   (gethash key (model-object model)))))

(defsetf agent-gethash agent-puthash)

(defun agent-remhash (key hash-agent)
  "Delete a hash-table entry. See `cl:remhash`.
Returns `t` if entry existed, `nil` otherwise."
  (let ((hash-table (agt:agent-get-quick hash-agent
                                         (lambda (model) (model-object model)))))
    (if (gethash key hash-table)
        (agt:agent-update hash-agent
                          (with-update-handler
                            (remhash key (model-object model))))
        nil)))

(defun agent-clrhash (hash-agent)
  "Clears the hash-table. See `cl:clrhash`."
  (agt:agent-update hash-agent
                    (with-update-handler
                      (clrhash (model-object model)))))

(defun agent-dohash (fun hash-agent)
  "'Do' arbitrary atomic operation on the hash-table.

`fun`: is a 1-arity function taking the hash-table. This function can operate on the hash-table without interference from other threads. The result of this function must be a hash-table.  
`hash-agent`: is the `hash-agent` instance.

The result of `agent-dohash` is `T`."
  (agt:agent-update hash-agent
                    (lambda (model)
                      (setf (model-object model)
                            (funcall fun (model-object model)))
                      model)))
