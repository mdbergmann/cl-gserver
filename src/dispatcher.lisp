
(in-package :cl-gserver.dispatcher)

(shadowing-import '(mesgb:message-box/bt
                    act:actor))

(export '(make-dispatcher
          make-dispatcher-worker))

(defun make-dispatcher (&key (num-workers 1))
  "Default constructor."
  (make-instance 'shared-dispatcher
                 :num-workers num-workers))

(defclass dispatcher-base () ()
  (:documentation
   "A `dispatcher' contains a pool of `actors' that operate as workers where work is dispatched to."))

;; ---------------------------------
;; Shared dispatcher
;; ---------------------------------

(defclass shared-dispatcher (dispatcher-base)
  ((router :initform (router:make-router :strategy :random)))
  (:documentation
   "A shared dispatcher.
The strategy to choose a worker is random."))

(defmethod initialize-instance :after ((self shared-dispatcher) &key (num-workers 1))
  (with-slots (router) self
    (loop :for n :from 1 :to num-workers
          :do (router:add-routee router (make-dispatcher-worker n)))))

(defmethod print-object ((obj shared-dispatcher) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (router) obj
      (format stream "workers: ~a, strategy: ~a"
              (length (router:routees router))
              (router:strategy-fun router)))))

(defmethod workers ((self shared-dispatcher))
  (with-slots (router) self
    (router:routees router)))

(defmethod shutdown ((self shared-dispatcher))
  (with-slots (router) self
    (router:stop router)))

(defmethod dispatch ((self shared-dispatcher) dispatch-exec-fun)
  (with-slots (router) self
    (router:ask-s router (cons :execute dispatch-exec-fun))))

(defmethod dispatch-async ((self shared-dispatcher) dispatch-exec-fun)
  (with-slots (router) self
    (router:tell router (cons :execute dispatch-exec-fun))))


;; ---------------------------------
;; the worker
;; ---------------------------------

(defclass dispatch-worker (actor) ()
  (:documentation
   "Specialized `actor' used as `worker' is the message `dispatcher'."))

(defun make-dispatcher-worker (num)
  "Constructor for creating a worker."
  (let ((worker (make-instance 'dispatch-worker
                               :receive #'receive
                               :name (utils:mkstr "dispatch-worker-" num))))
    (setf (act-cell:msgbox worker)  (make-instance 'message-box/bt
                                                   :max-queue-size 0))
    worker))

(defun receive (self message current-state)
  "The worker receive function."
  (assert (consp message) nil (format t "~a: Message must be a `cons'!" (act-cell:name self)))
  (case (car message)
    (:execute (cons (funcall (cdr message)) current-state))))
