(defpackage :cl-gserver.dispatcher
  (:use :cl)
  (:nicknames :disp)
  (:export #:shared-dispatcher
           #:make-dispatcher
           #:make-dispatcher-worker
           #:workers
           #:shutdown
           #:dispatch
           #:dispatch-async))

(in-package :cl-gserver.dispatcher)

(defgeneric shutdown (dispatcher)
  (:documentation
   "Shutting down the dispatcher and all workers."))

(defgeneric dispatch (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function (`dispatch-exec-fun') to a worker of the dispatcher to execute there.
`dispatch' does a `ask' to a `dispatcher' worker, which means this call will block."))

(defgeneric dispatch-async (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch-async' does a `tell' to a `dispatcher' worker and is asynchronous."))
