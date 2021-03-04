(defpackage :cl-gserver.dispatcher
  (:use :cl)
  (:nicknames :disp)
  (:export #:dispatcher-base
           #:shared-dispatcher
           #:make-dispatcher
           #:workers
           #:shutdown
           #:dispatch
           #:dispatch-async
           #:dispatch-worker
           #:make-dispatcher-worker))

(defun make-dispatcher (&key (num-workers 1))
  "Default constructor.
This creates a `shared-dispatcher` with `num-workers` number of workers.
Each worker is based on a `:pinned` actor meaning that it has it's own thread."
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
))

(in-package :cl-gserver.dispatcher)

(defgeneric workers (dispatcher)
  (:documentation
   "Returns the workers of this dispatcher.
But better do not touch them.
Only use the defined interface here to talk to them."))

(defgeneric shutdown (dispatcher)
  (:documentation
   "Shutting down the dispatcher and all workers."))

(defgeneric dispatch (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function (`dispatch-exec-fun') to a worker of the dispatcher to execute there.
`dispatch' does a `ask-s' to a `dispatcher' worker, which means this call will block."))

(defgeneric dispatch-async (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch-async' does a `tell' to a `dispatcher' worker and is asynchronous."))
