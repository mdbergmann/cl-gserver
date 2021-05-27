(defpackage :cl-gserver.dispatcher
  (:use :cl)
  (:nicknames :disp)
  (:export #:dispatcher-base
           #:shared-dispatcher
           #:make-dispatcher
           #:workers
           #:identifier
           #:dispatch
           #:dispatch-async
           #:dispatch-worker
           #:stop
           #:make-dispatcher-worker))

(in-package :cl-gserver.dispatcher)

(defgeneric workers (dispatcher)
  (:documentation
   "Returns the workers of this dispatcher.
But better do not touch them.
Only use the defined interface here to talk to them."))

(defgeneric stop (dispatcher)
  (:documentation
   "Stops the dispatcher. Stops all workers."))

(defgeneric dispatch (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function (`dispatch-exec-fun`) to a worker of the dispatcher to execute there.
`dispatch` does a `ask-s` to a `dispatcher` worker, which means this call will block."))

(defgeneric dispatch-async (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch-async` does a `tell` to a `dispatcher` worker and is asynchronous."))
