(defpackage :cl-gserver.agent.hash
  (:use :cl)
  (:nicknames :agthash)
  (:export #:make-hash-agent
           #:agent-gethash
           #:agent-remhash
           #:agent-clrhash)
  )

(in-package :cl-gserver.agent.hash)

;; !!! This is work in progress. Don't use. !!!

(defun make-hash-agent (context &key (dispatcher-id :shared) (hash-table-args nil))
  "Creates an agent that wraps a CL hash-table.  
`context`: something implementing `ac:actor-context` protocol like `asys:actor-system`. Specifying `nil` here creates an agent outside of an actor system. The user has to take care of that himself.  
`dispatcher-id`: a dispatcher. defaults to `:shared`.  
`hash-table-args`: pass through to `make-hash-table`."
  (agt:make-agent (lambda () (apply #'make-hash-table hash-table-args))
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
