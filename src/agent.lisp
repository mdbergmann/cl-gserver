(defpackage :cl-gserver.agent
  (:use :cl :cl-gserver :trivia :cl-gserver.utils :log4cl)
  (:export #:make-agent
           #:agent-get
           #:agent-update
           #:agent))

(in-package :cl-gserver.agent)

(defclass agent (gserver) ())

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
  (let ((state (funcall state-fun)))
    (make-instance 'agent :state state :dispatch-workers 1)))

(defun agent-get (agent get-fun)
  (call agent (cons :get get-fun)))

(defun agent-update (agent update-fun)
  (cast agent (cons :update update-fun)))
