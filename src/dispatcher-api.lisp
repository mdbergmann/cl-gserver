(defpackage :sento.dispatcher
  (:use :cl)
  (:nicknames :disp)
  (:export #:dispatcher-base
           #:shared-dispatcher
           #:make-dispatcher
           #:workers
           #:identifier
           #:throughput
           #:*default-throughput*
           #:dispatch
           #:dispatch-async
           #:dispatch-worker
           #:stop
           #:make-dispatcher-worker))

(in-package :sento.dispatcher)

(defparameter *default-throughput* 5
  "The number of queued messages a `mesgb:message-box/dp` handles in one run on
a dispatcher worker before it yields the worker, unless the dispatcher config
specifies `:throughput`. A larger value amortizes the dispatch over more
messages of a busy actor; a smaller value gives other actors on the same
dispatcher a turn sooner. A run of a busy actor holds a worker for at most
this many messages, so keep it small when handlers take long, or better,
run such actors on a `:pinned` or a dedicated dispatcher.")

(defgeneric workers (dispatcher)
  (:documentation
   "Returns the workers of this dispatcher.
But better do not touch them.
Only use the defined interface here to talk to them."))

(defgeneric throughput (dispatcher)
  (:documentation
   "Returns the number of queued messages a message-box using this dispatcher
handles in one run on a worker before it yields the worker.
Also defined on `mesgb:message-box/dp`, which takes the value from its
dispatcher unless created with an explicit `:throughput`.
See `*default-throughput*`."))

(defgeneric stop (dispatcher)
  (:documentation
   "Stops the dispatcher. Stops all workers."))

(defgeneric dispatch (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function (`dispatch-exec-fun`) to a worker of the dispatcher to execute there.
`dispatch` does a `ask-s` to a `dispatcher` worker, which means this call will block.
The parameter `dispatcher-exec-fun` if of the form: `(list (function <something>))`"))

(defgeneric dispatch-async (dispatcher dispatcher-exec-fun)
  (:documentation
   "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch-async` does a `tell` to a `dispatcher` worker and is asynchronous.
Returns `T` when the function was handed to a worker. Any other value means
it was not, for example `:stopped` when the chosen worker was stopped or
`nil` when the dispatcher has no workers; `mesgb:message-box/dp` relies on
this to know whether a run of its queue is pending."))
