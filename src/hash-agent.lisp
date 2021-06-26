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

(defun make-hash-agent (&key (context nil) (dispatcher-id :shared) (hash-table-keys nil))
  (agt:make-agent (lambda () (apply #'make-hash-table hash-table-keys))
                  context dispatcher-id))

(defun agent-puthash (key hash-agent value)
  (agt:agent-update hash-agent (lambda (hash-table)
                                 (setf (gethash key hash-table) value)
                                 hash-table))
  value)

(defun agent-gethash (key hash-agent)
  (agt:agent-get hash-agent (lambda (hash-table)
                              (gethash key hash-table))))

(defsetf agent-gethash agent-puthash)

(defun agent-remhash (key hash-agent)
  (let ((hash-table (agt:agent-get-quick hash-agent #'identity)))
    (if (gethash key hash-table)
        (agt:agent-update hash-agent (lambda (hash-table)
                                       (remhash key hash-table)
                                       hash-table))
        nil)))

(defun agent-clrhash (hash-agent)
  (agt:agent-update hash-agent (lambda (hash-table) (clrhash hash-table))))
