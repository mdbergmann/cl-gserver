(defpackage :cl-gserver.actor
  (:use :cl)
  (:nicknames :act)
  (:export #:make-actor
           #:actor-of
           #:actor
           #:tell
           #:ask-s
           #:ask
           #:become
           #:unbecome
           #:context
           #:path
           #:watch
           #:unwatch
           #:watchers))

(in-package :cl-gserver.actor)

(defgeneric make-actor (receive &key name state type)
  (:documentation
   "Default constructor of an `actor`.
Specify a custom actor class as the `:type` key which defaults to 'actor.
Say you have a custom actor `custom-actor` and want `make-actor` create an instance of it.
Then specify `:type 'custom-actor` on `make-actor` function.
If you have additional initializations to make you can do so in `initialize-instance`.

The `receive` parameter is a function that should take 3 parameters.
That is:  
1. the actor `instance` itself, 
2. the `message` and 
3. the `current-state` of the actor.

The `receive` can then decide how to handle the message.
In any case it has to return a `cons` of message to be sent back to caller (`car`), if applicable.
And the new state of the actor.  
I.e.: `(cons <my-response> <my-new-state>)`

If the operation was an `ask-s` or `ask` then the `car` part of the `cons` will be sent back to the caller.
In case of a `tell` operation there will be no response and the `car` of the `cons` is ignored,
if there is no sender. If there is a sender defined, then the `car` of the `cons` is sent to the sender.
It is possible to specify `:no-reply` in this case which has the effect that this result is _not_ sent to the sender even if one exists. This is for the case that the user wants to handle the state and the any notifications to a sender himself. It is useful when the `receive` message handler is executed in a special thread-pool, because long running operations within `receive` will block the message handling of the actor.

The `:no-reply` result works for `ask` and `tell`, because also `ask` is based on `tell`.
`ask-s` is really only useful if a synchronous result is required and should be avoided otherwise."))

(defgeneric tell (actor message &optional sender)
  (:documentation
   "Sends a message to the `actor`. `tell` is asynchronous. There is no result.
If a `sender` is specified a message result of the target actor of the `tell` will be sent back to the `sender`

Generally `tell` does not expect a response. But a `sender` can be specified as optionl parameter to `tell`.
If a `sender` is specified, then the message handling behavior will send the `car` of the `cons` result to the specified `sender`.

A `sender` can also be part of the message contract.

`tell` can be used in two environments:

1. outside an actor

By default this sends a message as fire & forget. Since this is not inside an actor, no actor can be inferred as `sender`. A `sender` can be defined as optional parameter as part of `tell`.

2. inside an actors as part of the `receive` function

As `sender` can be specified when `tell` is used inside of an actor. Currently the framework doesn't automatically infer the `sender` when no `sender` is explicitly specified.

=> This is a future enhancement."))

(defgeneric ask-s (actor message &key time-out)
  (:documentation
   "Sends a message to the `actor`. `ask-s` is synchronous and waits for a result.
Specify `timeout` if a message is to be expected after a certain time.
An `:handler-error` with `timeout` condition will be returned if the call timed out.

`ask-s` assumes, no matter if `ask-s` is issued from outside or inside an actor, that the response is delivered back to the caller. That's why `ask-s` does block the execution until the result is available. The `receive` function handler must specify the result as the `car` of the cons result."))

(defgeneric ask (actor message &key time-out)
  (:documentation
   "This returns a `future`.
Specify `timeout` if a message is to be expected after a certain time.
An `:handler-error` with `timeout` condition will be returned is the call timed out.

An `ask` is similar to a `ask-s` in that the caller gets back a result 
but it doesn't have to actively wait for it. Instead a `future` wraps the result.
However, the internal message handling is based on `tell`.
How this works is that the message to the target `actor` is not 'sent' using the callers thread
but instead an anonymous `actor` is started behind the scenes and this in fact makes tells
the message to the target `actor`. It does sent itself along as 'sender'.
The target `actor` tells a response back to the initial `sender`. When that happens and the anonymous `actor` received the response the `future` will be fulfilled with the `promise`."))

(defgeneric become (actor new-behavior)
  (:documentation
   "Changes the receive of the actor to the given `new-behavior` function.
The `new-behavior` function must accept 3 parameters: the actor instance, the message and the current state."))

(defgeneric unbecome (actor)
  (:documentation
   "Reverts any behavior applied via `become` back to the default `receive` function."))

(defgeneric context (actor)
  (:documentation
   "This is the `actor-context` every actor is composed of.
When the actor is created from scratch it has no `actor-context`.
When created through the `actor-context`s, or system's `actor-of` function an `actor-context` will be set."))

(defgeneric path (actor)
  (:documentation
   "The path of the actor, including the actor itself.
The path denotes a tree which starts at the system context."))

(defgeneric watch (actor watcher)
  (:documentation
   "Registers `watcher` as a watcher of `actor`.
Watching lets the watcher know about lifecycle changes of the actor being watched.
I.e.: when it stopped. The message being sent in this case is: `(cons :stopped actor-instance)`"))

(defgeneric unwatch (actor watcher)
  (:documentation
   "Unregisters `watcher` of `actor`."))

(defgeneric watchers (actor)
  (:documentation
   "Returns a list of watchers of `actor`."))

;; --------------------------------------
;; Convenience macro for creating actors
;; --------------------------------------

(defmacro actor-of ((context
                     &optional (name nil)
                     &key (dispatcher :shared) (state nil) (type 'actor))
                    &body body)
  "Simple interface for creating an actor.
This macro is not to confuse with the actor-context function `actor-of`.
Internally it calls `ac:actor-of`.
`context` is either an `actor-system`, an `actor-context`, or an `actor` (any type of actor).
The new actor is created in the given context.
- `name` is optional. Specify when a static name is needed.
- `:state` key can be used to initialize with a state.
- `:dispatcher` key can be used to define the message dispatcher manually.
  Options are `:shared` (default) and `:pinned`.
- `:type` can specify a custom actor class. See `make-actor` for more info."
  (let ((unwrapped-context (gensym)))
    `(let ((,unwrapped-context (etypecase ,context
                                 (asys:actor-system ,context)
                                 (ac:actor-context ,context)
                                 (act:actor (act:context ,context)))))
           (ac:actor-of ,unwrapped-context
             (lambda () (act:make-actor ,@body :state ,state :name ,name :type ',type))
             :dispatch-type ,dispatcher))))
