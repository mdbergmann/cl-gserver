(defpackage :cl-gserver.actor-context
  (:use :cl)
  (:nicknames :ac)
  (:export #:actor-context
           #:make-actor-context
           ;; protocol
           #:actor-of
           #:find-actors
           #:all-actors
           #:stop
           #:notify
           #:shutdown
           ;;
           #:system
           #:id
           ;; conditions
           #:actor-name-exists))
(in-package :cl-gserver.actor-context)

(defgeneric actor-of (context create-fun &key dispatcher-id queue-size)
  (:documentation "Creates and adds actors to the given context.

Both an `AC:ACTOR-SYSTEM` and an `ACT:ACTOR` are composed of an `AC:ACTOR-CONTEXT`.
When an `actor-system` is specified as context (`actor-system` implements parts of the protocol) then the new actor will be a new root actor.

When the new actor should be a child of another actor, then the `actor-context` of the (to be) parent `actor` should be specified.
Creating an actor via `actor-of` will also add the `actor-context` as watcher of the actor. This watching can be used for different purposes. Right now the `actor` is removed from the context when it was stopped.

Specify the dispatcher type (`dispatcher-id`) as either:

- `:shared` to have this actor use the shared message dispatcher of the context
- `:pinned` to have this actor run it's own message box thread (faster, but more resource are bound.)

Specify `queue-size` with:

- 0: for a unbounded queue
- gt 0: for a bounded queue (preferably a size > 100)"))

(defgeneric find-actors (context path &key test key)
  (:documentation "Returns actors to be found by the criteria of:

- `context`: an `AC:ACTOR-CONTEXT`, or an `ACT:ACTOR` or an `ASYS:ACTOR-SYSTEM` as all three implement `find-actors`.
- `path`: a path designator to be found. This can be just an actor name, like 'foo', then `find-actors` will only look in the given context for the actor. It can also be: 'foo/bar', a relative path, in whcih case `find-actors` will traverse the path (here 'bar' is a child of 'foo') to the last context and will try to find the actor by name there, 'bar' in this case. Also possible is a root path like '/user/foo/bar' which will start traversing contexts started from the root context, which is the actor-system.
- `test`: a 2-arity test function where the 1st argument is the `path`, the 2nd is the a result of the `key` function (which defaults to `ACT-CELL:NAME`, so the name of the actor). The default function for `test` is `STRING=`. However, in case of a multi-subpath `path` both `test` and `key` only apply to the last path component, which designates the actor name to be found.
- `key`: a 1-arie function applied on an actor instance. Defaults to `ACT-CELL:NAME`.

Depending on `test` function the last path component can be used as a wildcard when using a `test` function like `STR:STARTS-WITH-P` or `STR:CONTAINSP` for example."))

(defgeneric all-actors (context)
  (:documentation "Retrieves all actors of this context as a list"))

(defgeneric stop (context actor)
  (:documentation
   "Stops the given actor on the context. 
The context may either be an `actor-context`, or an `actor-system`.
The actor is then also removed from the context."))

(defgeneric shutdown (context)
  (:documentation
   "Stops all actors in this context.
When the context is an `actor-context` this still stop the actor context and all its actors.
For the `actor-system` it will stop the whole system with all actors."))

(defgeneric notify (context actor notification)
  (:documentation
   "Notify the `actor-context` about something that happened to an actor.
Current exists:

- `:stopped`: this will remove the actor from the context."))

;; -------------------------------------
;; Conditions
;; -------------------------------------

(define-condition actor-name-exists (error)
  ((name :initarg :name
         :reader name))
  (:report (lambda (condition stream)
             (format stream "Actor with name '~a' already exists!" (name condition)))))
