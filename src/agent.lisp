(defpackage :cl-gserver.agent
  (:use :cl :cl-gserver :cl-gserver.utils :log4cl)
  (:export #:make-agent
           #:agent-get
           #:agent-update
           #:agent))

(in-package :cl-gserver.agent)

(defclass agent (gserver) ()
  (:documentation
"Specialized `gserver' class called `agent'.
It is meant primarily to encapsulate state.
To access state it provides `agent-get' and `agent-update' to update state."))

(defmethod handle-call ((self agent) message current-state)
  (cond
    ((consp message)
     (case (car message)
       (:get (cons
              (funcall (cdr message) current-state)
              current-state))))))

(defmethod handle-cast ((self agent) message current-state)
  (cond
    ((consp message)
     (case (car message)
       (:update (cons
                 current-state
                 (funcall (cdr message) current-state)))))))

(defun make-agent (state-fun)
"Makes a new `agent' instance.
`state-fun' is a function that takes no parameter and provides the initial state of the `agent' as return value."
  (let ((state (funcall state-fun)))
    (make-instance 'agent :state state)))

(defun agent-get (agent get-fun)
"Gets the current state of the `agent'.
`get-fun' must accept on parameter. That is the current-state of the `agent'.
To return the current state `get-fun' may be just the `identity' function.
See `agent-test' for examples."
  (call agent (cons :get get-fun)))

(defun agent-update (agent update-fun)
"Updates the `agent' state.
`update-fun' must accept on parameter. That is the current state of the `agent'.
The return value of `update-fun' will be taken as the new state of the `agent'."
  (cast agent (cons :update update-fun)))
