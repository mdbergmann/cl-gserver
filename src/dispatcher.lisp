(defpackage :cl-gserver.dispatcher
  (:use :cl :cl-gserver :cl-gserver.dispatcher-api)
  (:nicknames :dispatcher)
  (:export #:dispatcher-bt))

(in-package :cl-gserver.dispatcher)

(defclass dispatcher-bt (dispatcher-base) ()
  (:documentation
   "A `dispatcher' is a pool of `gserver's that is used by `gservers' or `actors' to handle the
messages for the. This allows to have a very large number of `gservers' or `actors' because
they do not have their own message processing thread but rather use this facility to have that done for them."))

(defmethod initialize-instance :after ((self dispatcher-bt) &key)
  (with-slots (num-workers workers) self
    (setf workers 
          (loop for x from 1 to num-workers
                collect (make-instance 'dispatch-worker
                                       :name (utils:mkstr "dispatch-worker-" x)
                                       :max-queue-size 1000)))))

(defmethod dispatch ((self dispatcher-bt) fun)
  "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch' does a `call' to a `dispatcher' worker, which means this call will block.
The strategy to select a worker is random."
  (with-slots (workers num-workers) self
    (when workers
      (call (nth (random num-workers) workers) (cons :execute fun)))))

(defmethod dispatch-async ((self dispatcher-bt) fun)
  "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch-async' does a `cast' to a `dispatcher' worker and is asynchronous.
The strategy to select a worker is random."
  (with-slots (workers num-workers) self
    (when workers
      (cast (nth (random num-workers) workers) (cons :execute fun)))))

(defmethod shutdown ((self dispatcher-bt))
  "Stops all workers."
  (with-slots (workers) self
    (mapcar (lambda (worker) (cast worker :stop)) workers)))

;; ---------------------------------
;; the worker
;; ---------------------------------

(defclass dispatch-worker (gserver) ()
  (:documentation
   "Specialized `gserver' used as `worker' is the message `dispatcher'."))
(defmethod handle-cast ((self dispatch-worker) message current-state)
  (case (car message)
    (:execute (cons (funcall (cdr message)) current-state))))
(defmethod handle-call ((self dispatch-worker) message current-state)
  (case (car message)
    (:execute (cons (funcall (cdr message)) current-state))))
