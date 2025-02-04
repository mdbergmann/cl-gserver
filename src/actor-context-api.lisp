(defpackage :sento.actor-context
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
(in-package :sento.actor-context)

(defgeneric actor-of (context
                      &key receive init destroy dispatcher state type name
                      &allow-other-keys)
  (:documentation "Interface for creating an actor.

**!!! Attention:** this factory function wraps the `act:make-actor` functionality to something more simple to use. 
Using this function there is no need to use `act:make-actor`.

`context` is either an `asys:actor-system`, an `ac:actor-context`, or an `act:actor` (any type of actor).
The new actor is created in the given context.

- `:receive` is required and must be a 1-arity function where the arguments is received message object.
The function can be just a lambda like `(lambda (msg) ...)`.
- `:init`: is an optional initialization function with one argument: the actor instance (self).
This represents a 'start' hook that is called after the actor was fully initialized.
- `:destroy`: is an optional destroy function also with the actor instance as argument.
This function allows to unsubsribe from event-stream or such.
- `:state` key can be used to initialize with a state.
- `:dispatcher` key can be used to define the message dispatcher manually.
  Options that are available by default are `:shared` (default) and `:pinned`. When you defined a custom dispatcher it can be specified here.
- `:type` can specify a custom actor class. See `act:make-actor` for more info.
- `:name` to set a specific name to the actor, otherwise a random name will be used.

Additional options:

- `:queue-size` limits the message-box's size. By default, it is unbounded.
- `:mbox-type` specify a custom message-box type similar as can be done is dispatcher config.
  It must be a subtype of `mesgb:message-box/dp`.
"))

(defgeneric find-actors (context path &key test key)
  (:documentation "Returns actors to be found by the criteria of:

- `context`: an `AC:ACTOR-CONTEXT`, or an `ACT:ACTOR` or an `ASYS:ACTOR-SYSTEM` as all three implement `find-actors`.
- `path`: a path designator to be found. This can be just an actor name, like 'foo', then `find-actors` will only look in the given context for the actor. It can also be: 'foo/bar', a relative path, in which case `find-actors` will traverse the path (here 'bar' is a child of 'foo') to the last context and will try to find the actor by name there, 'bar' in this case. Also possible is a root path like '/user/foo/bar' which will start traversing contexts started from the root context, which is the actor-system.
- `test`: a 2-arity test function where the 1st argument is the `path`, the 2nd is the a result of the `key` function (which defaults to `ACT-CELL:NAME`, so the name of the actor). The default function for `test` is `STRING=`. However, in case of a multi-subpath `path` both `test` and `key` only apply to the last path component, which designates the actor name to be found.
- `key`: a 1-arity function applied on an actor instance. Defaults to `ACT-CELL:NAME`.

Depending on `test` function the last path component can be used as a wildcard when using a `test` function like `STR:STARTS-WITH-P` or `STR:CONTAINSP` for example."))

(defgeneric all-actors (context)
  (:documentation "Retrieves all actors of this context as a list"))

(defgeneric stop (context actor &key wait)
  (:documentation
   "Stops the given actor on the context. 
The context may either be an `actor-context`, or an `actor-system`.
The actor is then also removed from the context.
Specify `wait` as `T` to block until the actor is stopped (default `NIL`)."))

(defgeneric shutdown (context &key wait)
  (:documentation
   "Stops all actors in this context.
When the context is an `actor-context` this still stop the actor context and all its actors.
For the `actor-system` it will stop the whole system with all actors.
Specify `wait` as `T` to block until all actors of the context are stopped (default `NIL`)."))

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
