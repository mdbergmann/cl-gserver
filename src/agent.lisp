(defpackage :cl-gserver.agent
  (:use :cl :cl-gserver.actor)
  (:import-from #:cl-gserver.actor-cell
                #:running-p
                #:state
                #:msgbox)
  (:import-from #:mesgb
                #:message-box/bt)
  (:export #:make-agent           
           #:agent-get
           #:agent-update
           #:agent-stop
           #:agent))

(in-package :cl-gserver.agent)

(defclass agent (actor) ()
  (:documentation
   "Specialized `actor' class called `agent'.
It is meant primarily to encapsulate state.
To access state it provides `agent-get' and `agent-update' to update state.
Stop an agent with `agent-stop' to free resources (threads)."))

(defun receive (self message current-state)
  "This is the agents actor receive function implementation.
This rarely (if at all) needs to change because the agent is very specific."
  (declare (ignore self))
  (cond
    ((consp message)
     (case (car message)
       (:get (cons
              (funcall (cdr message) current-state)
              current-state))
       (:update (cons
                 current-state
                 (funcall (cdr message) current-state)))))))

(defun make-agent (state-fun)
  "Makes a new `agent' instance.
`state-fun' is a function that takes no parameter and provides the initial state of the `agent' as return value."
  (let* ((state (funcall state-fun))
         (agent (make-instance 'agent :state state
                                      :name (string (gensym "agent-"))
                                      :receive #'receive)))
    (setf (msgbox agent) (make-instance 'message-box/bt))
    agent))

(defun agent-get (agent get-fun)
  "Gets the current state of the `agent'.
`get-fun' must accept one parameter. That is the current-state of the `agent'.
To return the current state `get-fun' may be just the `identity' function.
Beware that this function does directly access the state of the agent for performance reasons.
It does not go through message processing.
See `agent-test' for examples."
  (with-slots (state) agent
    (if (running-p agent)
        (funcall get-fun state)
        :stopped)))

(defun agent-update (agent update-fun)
  "Updates the `agent' state.
`update-fun' must accept one parameter. That is the current state of the `agent'.
The return value of `update-fun' will be taken as the new state of the `agent'."
  (tell agent (cons :update update-fun)))

(defun agent-stop (agent)
  "Stops the message handling of the agent."
  (tell agent :stop))
