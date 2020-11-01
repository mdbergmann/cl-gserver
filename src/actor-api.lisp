(defpackage :cl-gserver.actor
  (:use :cl)
  (:nicknames :act)
  (:export #:make-actor
           #:actor
           #:tell
           #:ask
           #:async-ask
           #:context
           #:watch
           #:unwatch
           #:watchers)
)

(in-package :cl-gserver.actor)

(defgeneric make-actor (receive-fun &key name state)
  (:documentation
   "Default constructor of an `actor'.
The `receive-fun' parameter is a function that should take 3 parameters.
That is the actor instance itself, the message and the current state of the actor.
The `receive-fun' can then decide how to handle the message.
In any case it has to return a `cons' of message to be sent back to caller (`car'), if applicable.
And the new state of the actor.
I.e.: `(cons <my-response> <my-new-state>)'
If the operation was an `ask' or `async-ask' then the `car' part of the `cons' will be sent back to the caller.
In case of a `tell' operation there will be no response and the `car' of the `cons' is ignored."))

(defgeneric tell (actor message)
  (:documentation
   "Sends a message to the `actor'. `tell' is asynchronous. There is no result."))

(defgeneric ask (actor message &key timeout)
  (:documentation
   "Sends a message to the `actor'. `ask' is synchronous and waits for a result.
Specify `timeout' if a message is to be expected after a certain time.
An `:handler-error' with `timeout' condition will be returned is the call timed out."))

(defgeneric async-ask (actor message &key :timeout)
  (:documentation
   "This returns a `future'.
Specify `timeout' if a message is to be expected after a certain time.
An `:handler-error' with `timeout' condition will be returned is the call timed out.

An `async-ask' is similar to a `ask' in that the caller gets back a result 
but it doesn't have to actively wait for it. Instead a `future' wraps the result.
However, the internal message handling is based on `tell'.
How this works is that the message to the target `actor' is not 'sent' using the callers thread
but instead an anonymous `actor' is started behind the scenes and this in fact makes tells
the message to the target `actor'. It does sent itself along as 'sender'.
The target `actor' tells a response back to the initial `sender'. When that happens and the anonymous `actor'
received the response the `future' will be fulfilled with the `promise'."))

(defgeneric context (actor)
  (:documentation
   "This is the `actor-context' every actor is composed of.
When the actor is created from scratch it has no `actor-context'.
When created through the `actor-context's, or system's `actor-of' function an `actor-context' will be set."))

(defgeneric watch (actor watcher)
  (:documentation
   "Registers `watcher' as a watcher of `actor'.
Watching lets the watcher know about lifecycle changes of the actor being watched.
I.e.: when it stopped. The message being sent in this case is: `(cons :stopped actor-instance)`"))

(defgeneric unwatch (actor watcher)
  (:documentation
   "Unregisters `watcher' of `actor'."))

(defgeneric watchers (actor)
  (:documentation
   "Returns a list of watchers of `actor'."))
