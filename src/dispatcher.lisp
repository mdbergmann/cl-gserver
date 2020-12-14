
(in-package :cl-gserver.dispatcher)

(shadowing-import '(mesgb:message-box/bt
                    act:actor
                    act:tell
                    act:ask-s))

(export '(shared-dispatcher
          make-dispatcher
          make-dispatcher-worker
          workers))

(defun make-dispatcher (dispatcher-type &key (num-workers 1))
  "Default constructor."
  (make-instance dispatcher-type
                 :num-workers num-workers))

(defclass dispatcher-base ()
  ((workers :initform nil
            :reader workers
            :documentation "The workers of this dispatcher."))
  (:documentation
   "A `dispatcher' contains a pool of `actors' that operate as workers where work is dispatched to."))

(defmethod print-object ((obj dispatcher-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (workers) obj
      (format stream "workers: ~a" (length workers)))))

(defmethod initialize-instance :after ((self dispatcher-base) &key (num-workers 1))
  (with-slots (workers) self
    (setf workers 
          (loop for x from 1 to num-workers
                collect (make-dispatcher-worker x)))))

(defmethod shutdown ((self dispatcher-base))
  "Stops all workers."
  (with-slots (workers) self
    (mapcar (lambda (worker) (tell worker :stop)) workers)))

;; ---------------------------------
;; Shared dispatcher
;; ---------------------------------

(defclass shared-dispatcher (dispatcher-base) ()
  (:documentation
   "A shared dispatcher.
The strategy to choose a worker is random."))

(defmethod dispatch ((self shared-dispatcher) dispatch-exec-fun)
  (with-slots (workers) self
    (when workers
      (ask-s (nth (random (length workers)) workers) (cons :execute dispatch-exec-fun)))))

(defmethod dispatch-async ((self shared-dispatcher) dispatch-exec-fun)
  (with-slots (workers) self
    (when workers
      (tell (nth (random (length workers)) workers) (cons :execute dispatch-exec-fun)))))


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
