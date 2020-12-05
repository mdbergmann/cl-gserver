(defpackage :cl-gserver.actor
  (:use :cl)
  (:nicknames :act)
  (:export #:make-actor
           #:actor
           #:tell
           #:ask
           #:async-ask
           #:become
           #:unbecome
           #:context
           #:watch
           #:unwatch
           #:watchers))

(in-package :cl-gserver.actor)

(defgeneric make-actor (receive &key name state)
  (:documentation
   "Default constructor of an `actor'.
The `receive' parameter is a function that should take 3 parameters.
That is the actor instance itself, the message and the current state of the actor.
The `receive' can then decide how to handle the message.
In any case it has to return a `cons' of message to be sent back to caller (`car'), if applicable.
And the new state of the actor.
I.e.: `(cons <my-response> <my-new-state>)'
If the operation was an `ask' or `async-ask' then the `car' part of the `cons' will be sent back to the caller.
In case of a `tell' operation there will be no response and the `car' of the `cons' is ignored."))

(defgeneric tell (actor message &optional sender)
  (:documentation
   "Sends a message to the `actor'. `tell' is asynchronous. There is no result.
If a `sender' is specified a message result of the target actor of the `tell' will be sent back to the `sender'

Generally =tell= does not expect a response. But a 'sender' can be specified as optionl parameter to =tell=.
If a 'sender' is specified, then the message handling behavior will send the ~car~ of the ~cons~ result to the specified 'sender'.

A 'sender' can also be part of the message contract.

=tell= can be used in two environments:

1. outside an actor

By default this sends a message as fire & forget. Since this is not inside an actor, no actor can be inferred as 'sender'. A 'sender' can be defined as optional parameter as part of =tell=.

2. inside an actors as part of the 'receive' function

As 'sender' can be specified when =tell= is used inside of an actor. Currently the framework doesn't automatically infer the 'sender' when no 'sender' is explicitly specified.

=> This is a future enhancement."))

(defgeneric ask (actor message &key time-out)
  (:documentation
   "Sends a message to the `actor'. `ask' is synchronous and waits for a result.
Specify `timeout' if a message is to be expected after a certain time.
An `:handler-error' with `timeout' condition will be returned is the call timed out.

=ask= assumes, no matter if =ask= is issued from outside or inside an actor, that the response is delivered back to the caller. That's why =ask= does block the execution until the result is available. The 'receive' function handler must specify the result as the ~car~ of the ~cons~ result."))

(defgeneric async-ask (actor message &key time-out)
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

(defgeneric become (actor new-behavior)
  (:documentation
   "Changes the receive of the actor to the given `new-behavior' function.
The `new-behavior' function must accept 3 parameters: the actor instance, the message and the current state."))

(defgeneric unbecome (actor)
  (:documentation
   "Reverts any behavior applied via `become' back to the default `receive' function."))

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
