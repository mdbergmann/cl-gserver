(defpackage :sento.actor
  (:use :cl)
  (:nicknames :act)
  (:export #:make-actor
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
           #:watchers
           #:*self*
           #:*state*
           #:*sender*)
  (:import-from #:act-cell
                #:actor-cell))

(in-package :sento.actor)

(defclass actor (actor-cell)
  ((receive :initarg :receive
            :initform (error "'receive' must be specified!")
            :reader receive
            :documentation
            "`receive` is a function that has to take 1 parameter:
- `msg`: the received message
The `sender` of the message (if available) accessible with `act:*sender*` from within the receive function or a behavior.")
   (behavior :initform nil
             :documentation
             "Behavior function applied via `act:become` and reverted via `act:unbecome`
`act:behavior` function takes the same parameters as `act:receive`.")
   (init-fun :initarg :init
             :initform nil
             :documentation "Init hook.
Function is called when actor was initialized.
`act:context` is ready setup at that time.
Parameter of the lambda is the actor itself.")
   (destroy-fun :initarg :destroy
                :initform nil
                :documentation "Destroy hook.
Function is called when actor was stopped.
Parameter of the lambda is the actor itself.")
   (context :initform nil
            :accessor context
            :documentation "The `ac:actor-context`")
   (watchers :initform '()
             :reader watchers
             :documentation "List of watchers of this actor."))
  (:documentation
   "This is the `actor` class.

The `actor` does its message handling using the `receive` function.

The `receive` function takes one argument (the message). For backwards compatibility and for convenience it can still be used to provide an immediate return for `act:ask-s`. `act:tell` and `act:ask` ignore a return value.

There is asynchronous `tell`, a synchronous `ask-s` and asynchronous `ask` which all can be used to send messages to the actor. `ask-s` provides a synchronous return taken from the `receive` functions return value. 'ask' provides a return wrapped in a future. But the actor has to explicitly use `*sender*` to formulate a response. `tell` is just fire and forget.

To stop an actors message processing in order to cleanup resouces you should `tell` (or `ask-s`) the `:stop` message. It will respond with `:stopped` (in case of `ask(-s)`)."))

(defgeneric make-actor (receive &key name state type init destroy)
  (:documentation
   "Constructs an `actor`.

Arguments:

- `receive`: message handling function taking one argument, the message.

- `name`: give the actor a name. Must be unique within an `ac:actor-context`.

- `type`: Specify a custom actor class as the `:type` key. Defaults to 'actor.
Say you have a custom actor `custom-actor` and want `make-actor` create an instance of it.
Then specify `:type 'custom-actor` on `make-actor` function.
If you have additional initializations to make you can do so in `initialize-instance`.

- `state`: initialize an actor with a state. (default is `nil`)

- `init` and `destroy`: are functions that take one argument, the actor instance.
Those hooks are called on (after) initialization and (after) stop respectively."))

(defgeneric tell (actor message &optional sender)
  (:documentation
   "Sends a message to the `actor`. `tell` is asynchronous.
`tell` does not expect a result. If a `sender` is specified the receiver will be able to send a response."))

(defgeneric ask-s (actor message &key time-out)
  (:documentation
   "Sends a message to the `actor`. `ask-s` is synchronous and waits for a result.
Specify `timeout` if a message is to be expected after a certain time.
An `:handler-error` with `timeout` condition will be returned if the call timed out.

`ask-s` assumes, no matter if `ask-s` is issued from outside or inside an actor, that the response is delivered back to the caller. That's why `ask-s` does block the execution until the result is available. The `receive` function return value will be used as the result of `receive`."))

(defgeneric ask (actor message &key time-out)
  (:documentation
   "Sends a message to the `actor`. A `future` is returned.
Specify `timeout` if a message is to be expected after a certain time.
An `:handler-error` with `timeout` condition will be returned is the call timed out.

An `ask` is similar to a `ask-s` in that the caller gets back a result 
but it doesn't have to actively wait for it. Instead a `future` wraps the result.
However, the internal message handling is based on `tell`.
How this works is that the message to the target `actor` is not 'sent' using the callers thread but instead an anonymous `actor` is started behind the scenes. This anonymous actor can weit for a response from the target actor. The response then fulfills the future."))

(defgeneric become (new-behavior)
  (:documentation
   "Changes the receive of the actor to the given `new-behavior` function.
The `new-behavior` function must accept 3 parameters: the actor instance, the message and the current state.
This function should be called from within the behavior receive function."))

(defgeneric unbecome ()
  (:documentation
   "Reverts any behavior applied via `become` back to the default `receive` function.
This function should be called from within the behavior receive function."))

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
