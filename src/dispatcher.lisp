(defpackage :cl-gserver.dispatcher
  (:use :cl :cl-gserver.actor :cl-gserver.single-actor :cl-gserver.dispatcher-api)
  (:nicknames :dispatcher)
  (:export #:dispatcher-bt))

(in-package :cl-gserver.dispatcher)

(defclass dispatcher-bt (dispatcher-base) ()
  (:documentation
   "A `dispatcher' is a pool of `single-actor's that is used by `actors' or `agents' to handle the
messages for them. This allows to have a very large number of `actors' because
they do not have their own message processing thread but rather use this facility to have that done for them."))

(defmethod initialize-instance :after ((self dispatcher-bt) &key)
  (with-slots (num-workers workers) self
    (setf workers 
          (loop for x from 1 to num-workers
                collect (make-single-actor 'dispatch-worker
                                           :name (string (gensym "dispatch-worker-"))
                                           :queue-size 1000
                                           :receive-fun #'receive-fun)))))

(defmethod dispatch ((self dispatcher-bt) fun)
  "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch' does a `call' to a `dispatcher' worker, which means this call will block.
The strategy to select a worker is random."
  (with-slots (workers num-workers) self
    (when workers
      (ask (nth (random num-workers) workers) (cons :execute fun)))))

(defmethod dispatch-async ((self dispatcher-bt) fun)
  "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch-async' does a `cast' to a `dispatcher' worker and is asynchronous.
The strategy to select a worker is random."
  (with-slots (workers num-workers) self
    (when workers
      (tell (nth (random num-workers) workers) (cons :execute fun)))))

(defmethod shutdown ((self dispatcher-bt))
  "Stops all workers."
  (with-slots (workers) self
    (mapcar (lambda (worker) (tell worker :stop)) workers)))


;; ---------------------------------
;; the worker
;; ---------------------------------

(defclass dispatch-worker (actor) ()
  (:documentation
   "Specialized `gserver' used as `worker' is the message `dispatcher'."))

(defun receive-fun (self message current-state)
  (declare (ignore self))
  (case (car message)
    (:execute (cons (funcall (cdr message)) current-state))))
