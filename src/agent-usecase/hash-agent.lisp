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
                                  (initial-hash-table nil)
                                  (dispatcher-id :shared)
                                  (hash-table-args nil))
  "Creates an agent that wraps a CL hash-table.  
`context`: something implementing `ac:actor-context` protocol like `asys:actor-system`. Specifying `nil` here creates an agent outside of an actor system. The user has to take care of that himself.  
`initial-hash-table`: specify an initial hash-table. If not specified an empty one will be created.  
`dispatcher-id`: a dispatcher. defaults to `:shared`.  
`hash-table-args`: pass through to `make-hash-table`."
  (agt:make-agent (lambda ()
                    (if initial-hash-table initial-hash-table
                        (apply #'make-hash-table hash-table-args)))
                  context dispatcher-id))

(defun agent-puthash (key hash-agent value)
  "Internal"
  (agt:agent-update hash-agent (lambda (hash-table)
                                 (setf (gethash key hash-table) value)
                                 hash-table))
  value)

(defun agent-gethash (key hash-agent)
  "Retrieves value from hash-table, or `nil` if it doesn't exist.
See `cl:gethash` for more info.

This supports setting a hash using `setf` in the same way as with `cl:hash-table`."
  (agt:agent-get hash-agent (lambda (hash-table)
                              (gethash key hash-table))))

(defsetf agent-gethash agent-puthash)

(defun agent-remhash (key hash-agent)
  "Delete a hash-table entry. See `cl:remhash`.
Returns `t` if entry existed, `nil` otherwise."
  (let ((hash-table (agt:agent-get-quick hash-agent #'identity)))
    (if (gethash key hash-table)
        (agt:agent-update hash-agent (lambda (hash-table)
                                       (remhash key hash-table)
                                       hash-table))
        nil)))

(defun agent-clrhash (hash-agent)
  "Clears the hash-table. See `cl:clrhash`."
  (agt:agent-update hash-agent (lambda (hash-table) (clrhash hash-table))))

(defun agent-dohash (fun hash-agent)
  "'Do' arbitrary operations on the hash-table that is executed atomically.

`fun` is a 1-arity function taking the hash-table. This function can operate on the hash-table without interference from other threads. The result of this function is not relevant. It is ignored.  
`hash-agent` is the `hash-agent` instance.

The result of `dohash` is `T`."
  (agt:agent-update hash-agent (lambda (hash-table)
                                 (funcall fun hash-table)
                                 hash-table)))
