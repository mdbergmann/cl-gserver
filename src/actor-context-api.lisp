(defpackage :cl-gserver.actor-context
  (:use :cl)
  (:nicknames :ac)
  (:export #:actor-context
           #:make-actor-context
           #:actor-of
           #:find-actors
           #:all-actors
           #:stop
           #:notify
           #:shutdown
           #:system
           ;; conditions
           #:actor-name-exists))
(in-package :cl-gserver.actor-context)

(defclass actor-context ()
  ((actors :initform (make-array 50 :adjustable t :fill-pointer 0)
           :reader actors
           :documentation
           "A list of actors.
This is internal API. Use `all-actors' or `find-actors' instead.")
   (system :initform nil
           :reader system
           :documentation
           "A reference to the `actor-system'."))
  (:documentation "`actor-context' deals with creating and maintaining actors.
The `actor-system' and the `actor' itself are composed of an `actor-context'."))

(defgeneric actor-of (context create-fun &key dispatch-type)
  (:documentation "Creates and adds actors to the given context.
The context can be an `actor-context', an `actor-system',
because both an `actor-system' and an `actor' are composed of an `actor-context'.
When an `actor-system' is specified as context then the new actor will be a new root actor.
When th new actor should be a child of another actor, then the `actor-context' of the (to be) parent
`actor' should be specified.
Creating an actor via `actor-of' will also add the `actor-context' as watcher of the actor.

Specify the dispatcher type (`disp-type') as either:
`:shared' to have this actor use the shared message dispatcher of the context
`:pinned' to have this actor run it's own message box thread (faster, but more resource are bound.)"))

(defgeneric find-actors (context test-fun)
  (:documentation "Returns actors where `test-fun' provides 'truth'."))

(defgeneric all-actors (context)
  (:documentation "Retrieves all actors as a list"))

(defgeneric stop (context actor)
  (:documentation
   "Stops the given actor on the context. 
The context may either be an `actor-context', or an `actor-system'."))

(defgeneric notify (context actor notification)
  (:documentation
   "Notify the actor context about something.
Current exists:
- `:stopped'"))

(defgeneric shutdown (context)
  (:documentation
   "Stops all actors in this context.
When the context is an `actor-context' this still stop the actor context and all its actors.
For the `actor-system' it will stop the whole system with all actors."))

;; -------------------------------------
;; Conditions
;; -------------------------------------

(define-condition actor-name-exists (error)
  ((name :initarg :name
         :reader name))
  (:report (lambda (condition stream)
             (format stream "Actor with name '~a' already exists!" (name condition)))))
