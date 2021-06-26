(defpackage :cl-gserver.agent
  (:use :cl :cl-gserver.actor)
  (:nicknames :agt)
  (:import-from #:cl-gserver.actor-cell
                #:running-p
                #:state
                #:msgbox)
  (:import-from #:mesgb
                #:message-box/bt)
  (:export #:make-agent           
           #:agent-get
           #:agent-get-quick
           #:agent-update
           #:agent-update-and-get
           #:agent-stop
           #:agent))

(in-package :cl-gserver.agent)

(defclass agent (actor) ()
  (:documentation
   "Specialized `actor` class called `agent`.
It is meant primarily to encapsulate state.
To access state it provides `agent-get` and `agent-update` to update state.
Stop an agent with `agent-stop` to free resources (threads)."))

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
                 (funcall (cdr message) current-state)))
       (:update-and-get
        (let ((new-state (funcall (cdr message) current-state)))
          (cons new-state new-state)))))))

(defun make-agent (state-fun &optional actor-context (dispatcher-id :shared))
  "Makes a new `agent` instance.

`state-fun` is a function that takes no parameter and provides the initial state of the `agent` as return value.

`actor-context`: optionally specify an `asys:actor-system` as `actor-context`. If specified the agent will be registered in the `system` and destroyed with it should the `asys:actor-system` be destroyed. In addition the agent will use the systems shared message dispatcher and will _not_ create it's own.

`dispatcher-id`: the dispatcher is configurable. Default is `:shared`. But you may use also `:pinned` or a custom configured one. Be aware that `:shared` of a custom dispatcher only works if an `actor-context` was specified."
  (let* ((state (funcall state-fun))
         (creator-fun (lambda () (make-instance 'agent :state state
                                                  :name (string (gensym "agent-"))
                                                  :receive #'receive)))
         (agent (if actor-context
                    (ac:actor-of actor-context creator-fun :dispatcher-id dispatcher-id)
                    (funcall creator-fun))))
    (unless actor-context
      (setf (msgbox agent) (make-instance 'message-box/bt)))
    agent))

(defun agent-get-quick (agent get-fun)
  "Gets the current state with bypassing the messaging.
If you need consistent results this function should not be used."
  (with-slots (state) agent
    (if (running-p agent)
        (funcall get-fun state)
        :stopped)))

(defun agent-get (agent get-fun)
  "Gets the current state of the `agent`.
`get-fun` must accept one parameter. That is the current-state of the `agent`.
To return the current state `get-fun` may be just the `identity` function."
  (ask-s agent (cons :get get-fun)))

(defun agent-update (agent update-fun)
  "Updates the `agent` state.
`update-fun` must accept one parameter. That is the current state of the `agent`.
The return value of `update-fun` will be taken as the new state of the `agent`."
  (tell agent (cons :update update-fun)))

(defun agent-update-and-get (agent update-fun)
  "Updates the `agent` state.
`update-fun` must accept one parameter. That is the current state of the `agent`.
The return value of `update-fun` will be taken as the new state of the `agent`.
This function makes the update and returns the new value."
  (ask-s agent (cons :update-and-get update-fun)))

(defun agent-stop (agent)
  "Stops the message handling of the agent."
  (tell agent :stop))
