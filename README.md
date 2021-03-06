### Introduction - Actor framework featuring actors and agents

cl-gserver is a 'message passing' library/framework with actors
similar to Erlang or Akka.

**Version 1.4.1: changed documentation to the excellent [mgl-pax](https://github.com/melisgl/mgl-pax)**

**Version 1.4: convenience macro for creating actor. See below for more details**

**Version 1.3.1: round-robin strategy for router**

**Version 1.3: agents can be created in actor-system**

**Version 1.2 introduces a breaking change**

`ask` has been renamed to `ask-s`.

`async-ask` has been renamed to `ask`.

The proposed default way to query for a result from another actor should
be an asynchronous `ask`. `ask-s` (synchronous) is
of course still possible.

**Version 1.0** of `cl-gserver` library comes with quite a
few new features. One of the major new features is that an actor is not
bound to it's own message dispatcher thread. Instead, when an
`actor-system` is set-up, actors can use a shared pool of
message dispatchers which effectively allows to create millions of
actors.

It is now possible to create actor hierarchies. An actor can have child
actors. An actor now can also 'watch' another actor to get notified
about it's termination.

It is also possible to specify timeouts for the `ask-s` and
`ask` functionality.

This new version is closer to Akka (the actor model framework on the
JVM) than to GenServer on Erlang. This is because Common Lisp from a
runtime perspective is closer to JVM than to Erlang/OTP. Threads in
Common Lisp are heavy weight OS threads rather than user-space low
weight 'Erlang' threads (I'd like to avoid 'green threads', because
threads in Erlang are not really green threads). While on Erlang it is
easily possible to spawn millions of processes/threads and so each actor
(GenServer) has its own process, this model is not possible when the
threads are OS threads, because of OS resource limits. This is the main
reason for working with the message dispatcher pool instead.

But let's jump right into it. I'll explain more later.

### Getting hands-on

#### Creating an actor-system

To use the shared dispatcher pool we have to create an
`actor-system` first.

```elisp
(defvar *system* (asys:make-actor-system))
```

When we eval `*system*` in the repl we see a bit of the structure:

```plain
#<ACTOR-SYSTEM shared-workers: 4, user actors: 0, internal actors: 0>
```

So the `actor-system` has by default four shared message
dispatcher workers. Depending on how busy the system tends to be this
default can of course be increased.

1.  Shutting down the system

    Shutting down an actor system may be necessary depending on how
    it's used. It can be done by:

    ```elisp
    (ac:shutdown *system*)
    ```

    This will stop all dispatcher workers and all other actors that have
    been spawned in the system.

#### Creating actors

Actors kind of live within an `actor-context`. An
`actor-context` contains a collection (of actors) and defines a Common
Lisp protocol that spawns a set of generic functions.

There are two 'things' that host an `actor-context`. This
is:

1.  the `actor-system`. Creating actors on the
    `actor-system` will create root actors.
2.  the `actor`. Creating actors on the context of an actor
    will create a child actor.

Here we now use the `actor-context` protocol/API nicknamed
`ac`.

```elisp
(ac:actor-of *system* (lambda ()
                        (act:make-actor 
                             (lambda (self msg state)
                               (let ((output (format nil "Hello ~a" msg)))
                                 (format t "~a~%" output)
                                 (cons output state)))
                             :name "answerer")))
```

The convenience version is just this, which you can generally use instead of `ac:actor-of`:

```elisp
(act:actor-of (*system* "answerer")
  (lambda (self msg state)
    (let ((output (format nil "Hello ~a" msg)))
      (format t "~a~%" output)
      (cons output state))))
```

This creates a root actor on the `*system*`. Notice that the actor is
not assigned to a variable. It is now registered in the system. The main
argument to the `actor-of` function is a 'creator-function'
which when evaluated returns an actor created with the main actor
constructor `make-actor`.

`make-actor` requires as main parameter a 'receive'
function which should look familiar if you know the previous version of
cl-gserver. The parameters to the 'receive' function are still the
tuple of:

1.  `self` - the instance of the actor
2.  `msg` - the received message of when this 'receive'
    function is called
3.  `state` - the current state of the actor

`make-actor` also allows to specify the initial state, a
name, and a custom actor type. By default a standard actor of type
`'actor` is created. But you can subclass `'actor`
and specify your own. `make-actor` is still the facility to
create them. If you require custom initialization for the custom actor
do so in specializing `initialize-instance` function.

The return value of the 'receive' function should also be familiar. It
is the `cons` with `car` being sent back to sender
(in case of ask/ask-s) and `cdr` set as the new state of the
actor.

The function `actor-of` still returns the actor as can be
seen on the repl when this is executed. So it is of course possible to
store the actor in a dynamic or lexical context. However, when the
lexical context ends, the actor will still live as part of the actor
context.

Here we see a few details of the actor. Among which is the name and also
the type of message-box it uses. By default it is a
`message-box/dp` which is the type of a shared message
dispatcher message-box.

```
#<ACTOR answerer, running: T, state: NIL, message-box: #<MESSAGE-BOX/DP mesgb-9541, processed messages: 0, max-queue-size: 0, queue: #<QUEUE-UNBOUNDED #x3020029918FD>>>
```

Had we stored the actor to a variable, say `*answerer*` we
can create a child actor of that by doing:

```elisp
(ac:actor-of (act:context *answerer*) 
                          (lambda ()
                            (act:make-actor
                             (lambda (self msg state)
                               (let ((output (format nil "~a" "Hello-child ~a" msg)))
                                 (format t "~a~%" output)
                               (cons output state)))
                             :name "child-answerer")))
```

This will create a new actor on the context of the parent actor. The
context is retrieved with `(act:context *answerer*)`.

#####  Convenience macro for creating actors

The macro `actor-of` in the `actor` package
allows creating actors more easily. Internally it uses
`ac:actor-of` and `act:make-actor` functions.
But is removes a bit of typing. It also allows to specify either an
actor-system, an actor-context or just an actor as
`context` argument. The macro figures out the real
context required to create the actor. Similarly as for
`make-actor` it is possible to specify state, name and a
custom actor type to the macro.

Here is an example:

```elisp
(act:actor-of (*system*) 
  (lambda (self msg state)
    (cons "Hello world" state)))
```

It is sufficient to just specify the 'receive' lambda. The macro will add the rest.

##### Dispatchers `:pinned` vs. `:shared`

By default an actor created using `actor-of` uses a `:shared`
dispatcher type which uses the shared message dispatchers that are
setup in the system. It is also possible to create actors with their
own dispatcher thread, those are called `:pinned` actors. Just
provide the `:pinned` value to the `dispatcher-type` key parameter.

#### Finding actors in the context

If actors are not directly stored in a dynamic or lexical context they
can still be looked up and used. The `actor-context` protocol
contains a function `find-actors` which works like this:

```elisp
(first (ac:find-actors 
                 *system*
                 (lambda (actor) (string= "answerer" 
                                          (act-cell:name actor)))))
```

`find-actors` takes as first parameter the actor context.
This can be either the actor system, or the context of an actor. The
second parameter is a test function. This example makes a string
comparison on the actor name. So the above function will output:

```
#<ACTOR answerer, running: T, state: NIL, message-box: #<MESSAGE-BOX/DP mesgb-9687, processed messages: 0, max-queue-size: 0, queue: #<QUEUE-UNBOUNDED #x30200263C95D>>>
```

This function only does a simple flat search. The functionality of
looking up an actor in the system generally will be expanded upon.

#### tell, ask-s and ask

Let's send some messages.

##### tell

`tell` is a fire-and-forget kind of send type. It
doesn't expect a result in return.

And because of that, and in order to demonstrate it does something,
it has to have a side-effect. So it dumps some string to the console
using `format`, because we couldn't otherwise `tell` if
the message was received and processed (see the
`*answerer*` actor definitions above).

```elisp
CL-USER> (act:tell *answerer* "Foo")
T
CL-USER> 
Hello Foo
```

So we see that `tell` returns immediately with `T`. But
to see the 'Hello Foo' it takes another hit on the return key,
because the REPL is not asynchronous.

##### tell with sender

`tell` accepts a 'sender', which has to be an actor. So
we can do like this:

```elisp
CL-USER> (act:tell *child-answerer* "Foo" *answerer*)
T
CL-USER> 
Hello-child Foo
Hello Hello-child Foo
```

This sends \"Foo\" to `*child-answerer*`, but `*child-answerer*`
sends the response to `*answerer*`. So we see outputs of both
actors.

##### ask-s

`ask-s` blocks until the message was processed by the
actor. This call returns the `car` part of the `cons` return of the
behavior function. Insofar an `ask-s` call is more
resource intensive than just a `tell`.

```elisp
(act:ask-s *answerer* "Bar")
```

Will respond with: 'Hello Bar'

##### ask

`ask` combines both `ask-s` and
`tell`. From `ask-s` it 'inherits' returning
a result, even though it's a future result. Internally it is
implemented using `tell`. In order to wait for a result a
temporary actor is spawned that waits until it receives the result
from the actor where the message was sent to. With this received
result the future is fulfilled. So `ask` is async, it
returns immediately with a `future`. That
`future` can be queried until it is fulfilled. Better is
though to setup an `on-completed` handler function on it.

So we can do:

```elisp
(future:on-completed
          (act:ask *answerer* "Buzz")
          (lambda (result)
            (format t "Received result: ~a~%" result)))
```

Well, one step at a time:

```elisp
(act:ask *answerer* "Buzz")
```

Returns with:

```
#<FUTURE promise: #<PROMISE finished: NIL errored: NIL forward: NIL #x302002EAD6FD>>
```

Then we can setup a completion handler on the future:

```elisp
(future:on-completed 
          *
          (lambda (result)
            (format t "Received result: ~a~%" result)))
```

Remember '\*' is the last result in the REPL which is the future
here.

This will print after a bit:

```
Hello Buzz
Received result: Hello Buzz
```

#### ask-s and ask with timeout

A timeout (in seconds) can be specified for both `ask-s` and
`ask` and is done like so:

To demonstrate this we could setup an example 'sleeper' actor:

```elisp
(ac:actor-of *system* 
             (lambda () (act:make-actor 
                           (lambda (self msg state)
                             (sleep 5)))))
```

If we store this to `*sleeper*` and do the following, the
`ask-s` will return a `handler-error` with an
`ask-timeout` condition.

```elisp
(act:ask-s *sleeper* "Foo" :time-out 2)
```

```
(:HANDLER-ERROR . #<CL-GSERVER.UTILS:ASK-TIMEOUT #x30200319F97D>)
```

This works similar with the `ask` only that the future will
be fulfilled with the `handler-error` `cons`.

To get a readable error message of the condition we can do:

```
CL-USER> (format t "~a" (cdr *))
A timeout set to 2 seconds occurred. Cause: 
#<BORDEAUX-THREADS:TIMEOUT #x302002FAB73D> 
```

#### Long running operations in `receive`

Be careful with doing long running computations in the
`receive` function message handler, because it will block
message processing. It is advised to use a third-party thread-pool or a
library like *lparallel* to do the computations with and return early
from the `receive` message handler.

Considering the required `cons` return result of the
`receive` function, in case a result computation is delegated
to a thread-pool the `receive` function should return with
`(cons :no-reply <state>)`. The `:no-reply` will instruct the actor to
*not* send a result to a sender automatically should a sender be
available (for the cases of `tell` or `ask`). The
computation result can be 'awaited' for in an asynchronous manner and
a response to `*sender*` can be sent manually by just doing a
`(tell *sender* <my-computation-result>)`. The sender of the original
message is set to the dynamic variable `*sender*`.

Due to an asynchronous callback of a computation running is a separate
thread, the `*sender*` must be copied into a lexical environment because
at the time of when the callback is executed the `*sender*` can have a
different value.

This behavior must be part of the messaging protocol that is being
defined for the actors at play.

#### Changing behavior

An actor can change behavior. The behavior is just a lambda that has to
take three parameters:

1.  the actor's instance - usually called `self`
2.  the received message - maybe call `msg`?
3.  the current state of the actor

The behavior then can pattern match (or do some matching by other means)
on the received message alone, or in combination with the current state.

The default behavior of the actor is given on actor construction using
the default constructor `make-actor`.

During the lifetime of an actor the behavior can be changed using
`become`.

So we remember the `*answerer*` which responds with 'Hello Foo' when
we send `(act:ask-s *answerer* "Foo")`. We can now change the behavior
with:

```elisp
(act:become *answerer* 
            (lambda (self msg state)
              (cons (format nil "my new behavior for: ~a" msg) state)))
```

When we now send `(act:ask-s *answerer* "Foo")` we will get the
response: 'my new behavior for: Foo'.

**Reverting `become` / `unbecome`**

To revert back to the default behavior as defined by the
`receive` function of the constructor you may call
`unbecome`.

#### Creating actors without a system

It is still possible to create actors without a system. This is how you
do it:

```elisp
;; make an actor
(defvar *my-actor* (act:make-actor (lambda (self msg state)
                                     (cons "Foo" state))
                                   :name "Lone-actor"))
;; setup a thread based message box
(setf (act-cell:msgbox *my-actor*) 
      (make-instance 'mesgb:message-box/bt))
```

You have to take care yourself about stopping the actor and freeing
resources.

### Agents

An Agent is a specialized Actor. It is meant primarily for maintaining
state and comes with some conveniences to do that.

To use an Agent import `cl-gserver.agent` package.

There is no need to subclass an Agent. Rather create a facade to
customize an agent. See below.

An Agent provides three functions to use it.

-   `make-agent` creates a new agent
-   `agent-get` retrieves the current state of the agent. This directly
    delivers the state of the agent for performance reasons. There is no
    message handling involved.
-   `agent-update` updates the state of the agent

All three take a lambda. The lambda for `make-agent` does not take a
parameter. It should return the initial state of the agent. `agent-get`
and `agent-update` both take a lambda that must support one parameter.
This parameter represents the current state of the agent.

Let's make a simple example:

First create an agent with an initial state of `0`.

```elisp
(defparameter *my-agent* (make-agent (lambda () 0)))
```

Now update the state several times (`agent-update` is asynchronous and
returns `t` immediately):

```elisp
(agent-update *my-agent* (lambda (state) (1+ state)))
```

Finally get the state:

```elisp
(agent-get *my-agent* #'identity)
```

This `agent-get` just uses the `identity` function to return the state
as is.

So this simple agent represents a counter.

It is important to note that the retrieves state, i.e. with `identity`
should not be modified outside the agent.

#### Using an agent within an actor-system

The `make-agent` constructor function allows to provides an optional
`system` argument that, when given, makes the constructor create the
agent within the given actor-system. This implies that the systems
shared messages dispatcher is used for the agent and no separate thread
is created for the agents message box.

It also implies that the agent is destroyed then the actor-system is
destroyed.

However, while actors can create hierarchies, agents can not. Also the
API for creating agents in systems is different to actors. This is to
make explicit that agents are treated slightly differently than actors
even though under the hood agents are actors.

#### Wrapping an agent

While you can use the agent as in the example above it is usually
advised to wrap an agent behind a more simple facade that doesn't work
with lambdas.

For example could a facade for the counter above look like this:

```elisp
(defvar *counter-agent* nil)

(defun init-agent (initial-value)
  (setf *counter-agent* (make-agent (lambda () initial-value))))

(defun increment () (agent-update *counter-agent* #'1+))
(defun decrement () (agent-update *counter-agent* #'1-))
(defun counter-value () (agent-get *counter-agent* #'identity))
```

Alternatively, one can wrap an agent inside a class and provide methods
for simplified access to it.

### Router

A `Router` is a facade over a set of actors. Routers are
either created with a set of actors using the default constructor
`router:make-router` or actors can be added later.

Routers implement part of the actor protocol, so it allows to use
`tell`, `ask-s` or `ask` which it
forwards to a 'routee' (one of the actors of a router) by passing all
of the given parameters. The routee is chosen by applying a
`strategy`. The built-in default strategy a routee is chosen
randomly.

The `strategy` can be configured when creating a router using
the constructors `&key` parameter `:strategy`. The
`strategy` is just a function that takes the number of
routees and returns a routee index to be chosen for the next operation.

Currently available strategies: `:random` and
`:round-robin`.

Custom strategies can be implemented.

### Dispatchers

#### :shared

A `:shared` dispatcher is a separate facility that is set up in the `actor-system`. It consists of a configurable pool of 'dispatcher workers' (which are in fact actors). Those dispatcher workers execute the message handling in behalf of the actor and with the actors message handling code. This is protected by a lock so that every only one dispatcher will run code on an actor. This is to ensure protection from data race conditions of the state data of the actor (or other slots of the actor).

Using this dispatcher allows to create a large number of actors. The actors as such are generally very cheap.

<img alt="" src="./docs/disp_shared.png" width="700"/>
<img alt="" src="disp_shared.png" width="700"/>

#### :pinned

The `:pinned` dispatcher is represented by a thread that operates on the actors message queue. It handles one message after the other with the actors message handling code. This also ensures protection from data race conditions of the state of the actor.

This variant is slightly faster (see below) but requires one thread per actor.

<img alt="" src="./docs/disp_pinned.png" width="700"/>
<img alt="" src="disp_pinned.png" width="700"/>

### Benchmarks

![](./docs/perf.png)
![](perf.png)

Hardware specs:

-   iMac Pro (2017) with 8 Core Xeon, 32 GB RAM

**All**

The benchmark was created by having 8 threads throwing each 125k (1m
alltogether) messages at 1 actor. The timing was taken for when the
actor did finish processing those 1m messages. The messages were sent by
either all `tell`, `ask-s`, or `ask` to
an actor whose message-box worked using a single thread
(`:pinned`) or a dispatched message queue
(`:shared` / `dispatched`) with 8 workers.

Of course a `tell` is in most cases the fastest one, because
it's the least resource intensive and there is no place that is
blocking in this workflow.

**SBCL (v2.0.10)**

Even though SBCL is by far the fastest one with `tell` on
both `:pinned` and `dispatched`, it had massive
problems on `dispatched - ask-s` where I had to lower the
number of messages to 200k alltogether. Beyond that value SBCL didn't
get it worked out.

**CCL (v1.12)**

CCL is on acceptable average speed. The problems CCL had was heap
exhaustion for both the `ask` tasks where the number of
messages had to be reduced to 80k. Which is not a lot. Beyond this value
the runtime would crash. However, CCL for some reason had no problems
where SBCL was struggling with the `dispatched - ask-s`.

**ABCL (1.8)**

The pleasant surprise was ABCL. While not being the fastest it is the
most robust. Where SBCL and CCL were struggling you could throw anything
at ABCL and it'll cope with it. I'm assuming that this is because of
the massively battle proven Java Runtime.
