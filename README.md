![CI](https://github.com/mdbergmann/cl-gserver/workflows/CI/badge.svg?branch=master)

### Introduction - Actor framework featuring actors and agents

__Sento__ is a 'message passing' library/framework with actors similar to Erlang or Akka. It supports creating systems that should work reactive, require parallel computing and event based message handling.

Sento features:

- Actors with `ask` (`?`) and `tell` (`!`) operations. `ask` can be asynchronous or synchronous.
- Agents: Agents are a specialization of Actors for wrapping state with a standardized interface of `init`, `get` and `set`. There are also specialized Agents for Common Lisps array and hash-map data structures.
- FSM (Finite-State-Machine). A ready fsm framework.
- Router: Router offers a similar interface as Actor with `ask` and `tell` but collects multiple Actors for load-balancing.
- EventStream: all Actors and Agents are connected to an EventStream and can subscribe to messages or publish messages. This is similar to an event-bus.
- Tasks: a simple API for concurrency.
- Futures library

(Please also checkout the API [documentation](https://mdbergmann.github.io/cl-gserver/index.html) for further information)
(for migrations from Sento v2, please check below migration guide)

### Projects using Sento (for example usage):

- [Chipi automation tool](https://github.com/mdbergmann/chipi): Actors used for foundational primitives like 'items' and 'persistences'.
- [KNX-conn](https://github.com/mdbergmann/knx-conn): Used for asynchronous reading/writing from/to KNX bus
- [Hunchentoot taskmanager](https://github.com/mdbergmann/cl-tbnl-gserver-tmgr): High throughput Hunchentoot task manager

### Intro

#### Creating an actor-system

The first thing you wanna do is to create an actor system.
In simple terms, an actor system is a container where all actors live in. So at any time the actor system knows which actors exist.

To create an actor system we can first change package to `:sento-user` because it imports the majority of necessary namespaces fopr convenience. Then, do:

```elisp
(defvar *system* (make-actor-system))
```

When we look at `*system*` in the repl we see some information of the actor system:

```plain
#<ACTOR-SYSTEM config: (DISPATCHERS
                        (SHARED (WORKERS 4 STRATEGY RANDOM))
                        TIMEOUT-TIMER
                        (RESOLUTION 500 MAX-SIZE 1000)
                        EVENTSTREAM
                        (DISPATCHER-ID SHARED)
                        SCHEDULER
                        (ENABLED TRUE RESOLUTION 100 MAX-SIZE 500)
                        ), user actors: 0, internal actors: 5>
```

The `actor-system` has, by default, four shared message dispatcher workers. Depending on how busy the system tends to be this default can be increased. Those four workers are part of the 'internal actors'. The 5th actor drives the event-stream (later more on that, but in a nutshell it's something like an event bus).

There are none 'user actors' yet, and the 'config' is the default config specifying the number of message dispatch workers (4) and the strategy they use to balance throughput, 'random' here.

Using a custom config is it possible to change much of those defaults. For instance, create custom dispatchers, i.e. a dedicated dispatcher used for the 'Tasks' api (see later for more info). The event-stream by default uses the global 'shared' dispatcher. Changing the config it would be possible to have the event-stream actor use a `:pinned` dispatcher (more on dispatchers later) to optimize throughput. Etc.

Actors live in the actor system, but more concrete in an `actor-context`. An `actor-context` contains a collection (of actors) and represents a Common Lisp protocol that defines a set of generic functions for creating, removing and finding actors in an `actor-context`. The actor system itself is also implementing the `actor-context` protocol, so it also acts as such and hence the protocol `ac` (`actor-context`) is used to operate on the actor system.

I.e. to shutdown the actor system one has to execute: `(ac:shutdown *system*)`.


#### Creating and using actors

Now we want to create actors.

```elisp
(actor-of *system* :name "answerer"
  :receive
  (lambda (msg)
    (let ((output (format nil "Hello ~a" msg)))
        (reply output))))
```

This creates an actor in `*system*`. Notice that the actor is not assigned to a variable (but you can). It is now registered in the system. Using function `ac:find-actors` you'll be able to find it again. Of course it makes sense to store important actors that are frequently used in a `defparameter` variable.

The `:receive` key argument to `actor-of` is a function which implements the message processing behaviour of an actor. The parameter to the 'receive' function is just the received message (msg).

`actor-of` also allows to specify the initial state, a name, and a custom actor type via key parameters. By default a standard actor of type `'actor` is created. It is possible to subclass `'actor` and specify your own. It is further possible to specify an 'after initialization' function, using the `:init` key, and 'after destroy' function using `:destroy` keyword. `:init` can, for example, be used to subscribe to the event-stream for listening to important messages.

The return value of 'receive' function is only used when using the synchronous `ask-s` function to 'ask' the actor. Using `ask` (equivalent: `?`) the return value is ignored. If an answer should be provided to an asking actor, or if replying is part of an interface contract, then `reply` should be used.

Store the actor to a variable `*answerer*` by just doing `(defparameter *answerer* *)` in the repl where `*` resembled the last result (the actor instance in our case). By evaluating the variable (`*answerer*`) we can see the printed object:

```
#<ACTOR path: /user/answerer, cell: #<ACTOR answerer, running: T, state: NIL, message-box: #<SENTO.MESSAGEB:MESSAGE-BOX/DP mesgb-1356, processed messages: 1, max-queue-size: 0, queue: #<SENTO.QUEUE:QUEUE-UNBOUNDED 82701A6D13>>>>
```

We'll see the 'path' of the actor. The prefix '/user' means that the actor was created in a user actor context of the actor system. Further we see whether the actor is 'running', its 'state' and the used 'message-box' type, by default it uses an unbounded queue.

Now, when sending a message using 'ask' pattern to the above actor like so:

```elisp
(? *answerer* "FooBar")
```

we'll get a 'future' as result, because `?`/`ask` is asynchronous.

```plain
#<FUTURE promise: #<BLACKBIRD-BASE:PROMISE
finished: NIL
errored: NIL
forward: NIL 80100E8B7B>>
```

We can check for a 'future' result. By now the answer from the `*answerer*` (via `reply`) should be available:

```plain
USER> (fresult *)
"Hello FooBar"
```

If the reply had not been received yet, `fresult` would return `:not-ready`. So, `fresult` doesn't block, it is necessary to repeatedly probe using `fresult` until result is other than `:not-ready`.

A nicer and asynchronous way without querying is to use `fcompleted`. Using `fcompleted` you setup a callback function that is called with the result when it is available. Like this:

```elisp
(fcompleted
     (? *answerer* "Buzz")
     (result)
   (format t "The answer is: ~a~%" result))
```

Which will asynchronously print _"The answer is: Hello Buzz"_ after a short while.
This will also work when the `ask`/`?` was used with a timeout, in which case `result` will be a tuple of `(:handler-error . <ask-timeout condition>)` if the operation timed out.


#### Creating child actors

To build actor hierarchies one has to create actors in actors. This is of course possible. There are two options for this.

1. Actors are created as part of `actor-of`s `:init` function like so:

```elisp
(actor-of *system* 
          :name "answerer-with-child"
          :receive
          (lambda (msg)
            (let ((output (format nil "Hello ~a" msg)))
              (reply output)))
          :init
          (lambda (self)
            (actor-of self 
                      :name "child-answerer"
                      :receive 
                      (lambda (msg)
                        (let ((output (format nil "Hello-child ~a" msg)))
                          (format nil "~a~%" output))))))
```

Notice the context for creating 'child-answerer', it is `self`, which is 'answerer-with-child'.

2. Or it is possible externally like so:

```elisp
(actor-of *answerer* :name "child-answerer"
    :receive 
    (lambda (msg)
        (let ((output (format nil "~a" "Hello-child ~a" msg)))
            (format nil "~a~%" output))))
```

This uses `*answerer*` context as parameter of `actor-of`. But has the same effect as above.

Now we can check if there is an actor in context of 'answerer-with-child':

```plain
USER> (all-actors *actor-with-child*)
(#<ACTOR path: /user/answerer-with-child/child-answerer, cell: #<ACTOR child-answerer, running: T, state: NIL, message-box: #<SENTO.MESSAGEB:MESSAGE-BOX/DP mesgb-1374, processed messages: 0, max-queue-size: 0, queue: #<SENTO.QUEUE:QUEUE-UNBOUNDED 8200A195FB>>>>)
```

The 'path' is what we expected: '/user/answerer-with-child/child-answerer'.

#### Ping Pong

Another example that only works with `tell`/`!` (fire and forget).

We have those two actors.

The 'ping' actor:

```elisp
(defparameter *ping*
  (actor-of *system*
            :receive
            (lambda (msg)
              (cond
                ((consp msg)
                 (case (car msg)
                   (:start-ping
                    (progn
                      (format t "Starting ping...~%")
                      (! (cdr msg) :ping *self*)))))
                ((eq msg :pong)
                 (progn
                   (format t "pong~%")
                   (sleep 2)
                   (reply :ping)))))))
```

And the 'pong' actor:

```elisp
(defparameter *pong*
  (actor-of *system*
            :receive
            (lambda (msg)
              (case msg
                (:ping
                 (progn
                   (format t "ping~%")
                   (sleep 2)
                   (reply :pong)))))))
```

The 'ping' actor understands a `:start-ping` message which is a `cons` and has as `cdr` the 'pong' actor instance.
It also understands a `:pong` message as received from 'pong' actor.

The 'pong' actor only understands a `:ping` message. Each of the actors respond with either `:ping` or `:pong` respectively after waiting 2 seconds.

We trigger the ping-pong by doing:

```elisp
(! *ping* `(:start-ping . ,*pong*))
```

And then see in the console like:

```plain
Starting ping...
ping
pong
ping
...
```

To stop the ping-pong one just has to send `(! *ping* :stop)` to one of them.

##### Stopping actors

Sending the:
- `:stop` message will completely stop the actors message processing. No new messages will be accepted, but all messages in the queue are still processed.
- `:terminate` message will also stop the actor from accepting more messages but it will also discard any queued messages. Only the one that is currently being processed will be allowed to finish.

It is also possible to call `act-cell:stop` method on the actor. It has the same effect as sending `:terminate`.

##### Synchronous ask

At last an example for the synchronous 'ask', `ask-s`. It is insofar similar to `ask` that it provides a result to the caller. However, it is not bound to `reply` as with `ask`. Here, the return value of the 'receive' function is returned to the caller, and `ask-s` will block until 'receive' function returns.  
Beware that `ask-s` will dead-lock your actor when `ask-s` is used to call itself.  
Let's make an example:

```elisp
(defparameter *s-asker*
  (actor-of *system*
            :receive
            (lambda (msg)
              (cond
                ((stringp msg)
                 (format nil "Hello ~a" msg))
                (t (format nil "Unknown message!"))))))
```

So we can do:

```plain
USER> (ask-s *s-asker* "Foo")
"Hello Foo"
USER> (ask-s *s-asker* 'foo)
"Unknown message!"
```

#### Dispatchers `:pinned` vs. `:shared`

Dispatchers are somewhat alike thread pools. Dispatchers of the `:shared` type are a pool of workers. Workers are actors using a `:pinned` dispatcher. `:pinned` just means that an actor spawns its own mailbox thread.

So `:pinned` and `:shared` are types of dispatchers. `:pinned` spawns its own mailbox thread, `:shared` uses a worker pool to handle the mailbox messages.

By default an actor created using `actor-of` uses a `:shared` dispatcher type which uses the shared message dispatcher that is automatically setup in the system.

When creating an actor it is possible to specify the `dispatcher-id`. This parameter specifies which 'dispatcher' should handle the mailbox queue/messages.

Please see below for more info on dispatchers.

#### Finding actors in the context

If actors are not directly stored in a dynamic or lexical context they can still be looked up and used. The `actor-context` protocol contains a function `find-actors` which can lookup actors in various ways. Checkout the API [documentation](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.ACTOR-CONTEXT:FIND-ACTORS%20GENERIC-FUNCTION).

#### Mapping futures with fmap

Let's asume we have such a simple actor that just increments the value passed to it.

```
(defparameter *incer*
  (actor-of *system*
            :receive (lambda (value)
                       (reply (1+ value)))))
```

Since `ask` returns a future it is possible to map multiple `ask` operations like this:

```
(-> (ask *incer* 0)
  (fmap (result)
      (ask *incer* result))
  (fmap (result)
      (ask *incer* result))
  (fcompleted (result)
      (format t "result: ~a~%" result)
    (assert (= result 3))))
```


#### ask-s and ask with timeout

A timeout (in seconds) can be specified for both `ask-s` and
`ask` and is done like so:

To demonstrate this we could setup an example 'sleeper' actor:

```elisp
(ac:actor-of *system* 
    :receive 
    (lambda (msg)
        (sleep 5)))
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

Note that `ask-s` uses the calling thread for the timeout checks.  
`ask` uses a wheel timer to handle timeouts. The default resolution for `ask` timeouts is 500ms with a maximum size of wheel slots (registered timeouts) of 1000. What this means is that you can have timeouts of a multiple of 500ms and 1000 `ask` operations with timeouts. This default can be tweaked when creating an actor-system, see API [documentation](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.ACTOR-SYSTEM:*DEFAULT-CONFIG*%20VARIABLE) for more details.

#### Long running and asynchronous operations in `receive`

Be careful with doing long running computations in the `receive` function message handler, because it will block message processing. It is advised to use a separate thread, third-party thread-pool, a library like *lparallel*, or the provided 'Tasks' API using a dedicated dispatcher to do the computations with, and return early from the `receive` message handler.

The computation result can be 'awaited' for in an asynchronous manner and a response to `*sender*` can be sent manually (via `reply`). The sender of the original message is set to the dynamic variable `*sender*`.

Due to an asynchronous callback of a computation running is a separate thread, the `*sender*` must be copied into a lexical environment because at the time of when the callback is executed the `*sender*` can have a different value.

For instance, if there is a potentially long running and asynchronous operation happening in 'receive', the _original_ sender must be captured and the async operation executed in a lexical context, like so (receive function):

```elisp
(lambda (msg)
  (case msg
    (:do-lengthy-op
     (let ((sender *sender*))
       ;; do lengthy computation
       (reply :my-later-reply sender)))
    (otherwise
      ;; do other non async stuff
      (reply :my-reply))))
```

Notice that for the lengthy operation the sender must be captured because if the lengthy operation is asynchronous 'receive' function is perhaps called for another message where `*sender*` is different. In that case `sender` must be supplied explicitly for `reply`.

See [this test](../tests/spawn-in-receive-test.lisp) for more info.

NOTE: you should not change actor state from within an asynchronously executed operation in `receive`. This is not thread-safe. The pattern for this case it to send a message to `self` and have a message handler case that will change the actor state. This will ensure that actor state is always changed in a thread-safe way.

#### Changing behavior

An actor can change its behavior. The behavior is just a lambda similar as the 'receive' function taking the message as parameter.

The default behavior of the actor is given on actor construction using `:receive` key.

During the lifetime of an actor the behavior can be changed using `become`. `unbecome` will restore the default behavior.

Here is an example:

```elisp
(ac:actor-of *system*
             :receive
             (lambda (msg)
               (case msg
                 (:open
                  (progn
                    (unstash-all)
                    (become (lambda (msg)
                              (case msg
                                (:write
                                 ;; do something
                                 )
                                (:close
                                 (unbecome))
                                (otherwise
                                 (stash msg)))))))
                 (otherwise (stash msg)))))
```

#### Stashing messages

Stashing allows the actor to `stash` away messages for when the actor is in a state that doesn't allow it to handle certain messages. `unstash-all` can unstash all stashed messages.

See: API [documentation](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.STASH:STASHING%20CLASS) for more info.

#### Creating actors without a system

It is still possible to create actors without a system. This is how you do it:

```elisp
;; make an actor
(defvar *my-actor* (act:make-actor (lambda (msg)
                                     (format t "FooBar"))
                                   :name "Lone-actor"))
;; setup a thread based message box
(setf (act-cell:msgbox *my-actor*) 
      (make-instance 'mesgb:message-box/bt))
```

You have to take care yourself about stopping the actor and freeing resources.

### Agents

An Agent is a specialized Actor. It is meant primarily for maintaining state and comes with some conveniences to do that.

To use an Agent import `sento.agent` package.

There is no need to subclass an Agent. Rather create a facade to customize an agent. See below.

An Agent provides three functions to use it.

- `make-agent` creates a new agent. Optionally specify an `actor-context` or define the kind of dispatcher the agent should use.
- `agent-get` retrieves the current state of the agent. This directly delivers the state of the agent for performance reasons. There is no message handling involved.
- `agent-update` updates the state of the agent
- `agent-update-and-get` updates the agent state and returns the new state.

All four take a lambda. The lambda for `make-agent` does not take a parameter. It should return the initial state of the agent. `agent-get` and `agent-update` both take a lambda that must support one parameter.
This parameter represents the current state of the agent.

Let's make a simple example:

First create an agent with an initial state of `0`.

```elisp
(defparameter *my-agent* (make-agent (lambda () 0)))
```

Now update the state several times (`agent-update` is asynchronous and returns `t` immediately):

```elisp
(agent-update *my-agent* (lambda (state) (1+ state)))
```

Finally get the state:

```elisp
(agent-get *my-agent* #'identity)
```

This `agent-get` just uses the `identity` function to return the state as is.

So this simple agent represents a counter.

It is important to note that the retrieves state, i.e. with `identity` should not be modified outside the agent.

#### Using an agent within an actor-system

The `make-agent` constructor function allows to provide an optional `actor-context` argument that, when given, makes the constructor create the agent within the given actor-context. Another parameter `dispatcher-id` allows to specify the dispatcher where `:shared` is the default, `:pinned` will create the agent with a separate mailbox thread.

It also implies that the agent is destroyed then the actor-system is destroyed.

However, while actors can create hierarchies, agents can not. Also the API for creating agents in systems is different to actors. This is to make explicit that agents are treated slightly differently than actors even though under the hood agents are actors.

#### Wrapping an agent

While you can use the agent as in the example above it is usually advised to wrap an agent behind a more simple facade that doesn't work with lambdas and allows a more domain specific naming.

For example could a facade for the counter above look like this:

```elisp
(defvar *counter-agent* nil)

(defun init-agent (initial-value)
  (setf *counter-agent* (make-agent (lambda () initial-value))))

(defun increment () (agent-update *counter-agent* #'1+))
(defun decrement () (agent-update *counter-agent* #'1-))
(defun counter-value () (agent-get *counter-agent* #'identity))
```

Alternatively, one can wrap an agent inside a class and provide methods for simplified access to it.

### Finite State Machine (FSM)

The Finite State Machine (FSM) model is a computational framework designed to model and manage systems that transition between various states based on inputs or events. This structured approach facilitates the handling of complex logic through defined state transitions and events.

#### Creating an FSM

To create an FSM, use the `make-fsm` function, which initializes an actor with state management capabilities.

(Additional API documentation can be found [here](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.FSM:@FSM%20MGL-PAX:SECTION).)
(The API is quite stable, but since this is a new feature minor changes are possible.)

1. **actor-context**: Specifies where the FSM is created, which can be an actor, an actor-context, or an actor-system.
2. **name**: A string that names the FSM.
3. **start-with**: A cons cell representing the initial state and associated data.
4. **event-handling**: A function structured using FSM macros to define the FSM's behavior upon receiving events.

#### Example: Traffic Light Controller FSM with Timeouts

This FSM simulates a traffic light controller and includes comprehensive state and transition handling, including timeouts.

```elisp
(defun make-traffic-light-fsm (actor-context)
  (make-fsm
   actor-context
   :name "traffic-light-fsm"
   :start-with '(red . ())
   :event-handling (lambda ()

                     ;; Define behavior in each state using when-state
                     (when-state ('red :timeout-s 10)
                       (on-event ('timer) :state-timeout
                         (goto-state 'green))
                       (on-event ('manual-override)
                         (stay-on-state '(:manual-override-engaged))
                         (log:info "Manual override activated")))

                     (when-state ('green :timeout-s 15)
                       (on-event ('timer) :state-timeout
                         (goto-state 'yellow)))

                     (when-state ('yellow :timeout-s 5)
                       (on-event ('emergency-stop)
                         (goto-state 'red)
                         (log:info "Emergency stop activated"))
                       (on-event ('timer) :state-timeout
                         (goto-state 'red)))

                     ;; Handle state transitions
                     (on-transition ('(red . green))
                       (log:info "Transition from red to green"))

                     (on-transition ('(green . yellow))
                       (log:info "Transition from green to yellow"))

                     (on-transition ('(yellow . red))
                       (log:info "Transition from yellow to red"))

                     ;; Stay on current state but update data
                     (on-event ('maintenance-check)
                       (stay-on-state '(:maintenance-required))
                       (log:info "Maintenance required"))

                     ;; Handle unhandled events with specific and general catch
                     (when-unhandled ('unexpected-event)
                       (log:warn "Unexpected specific event caught"))

                     (when-unhandled (t :test #'typep)
                       (log:warn "Unhandled event caught: ~A" *received-event*)))))
```

#### Example Breakdown

- **actor-context**: Passed argument where the FSM operates.
- **name**: The FSM is named "traffic-light-fsm".
- **start-with**: The FSM begins in the 'red' state.
- **event-handling**: Utilizes various macros:
  - `when-state`: Defines actions specific to each state and handles timeouts.
  - `on-event ('timer)`: Transitions the FSM to the next state on timer events; handles timeouts with `:state-timeout`.
  - `goto-state`: Transitions the FSM to a new state, optionally updating data and logging.
  - `stay-on-state ('maintenance-check)`: Remains in the current state while updating state data.
  - `on-transition`: Logs state transitions.
  - General `when-unhandled` using `typep`: Catches all other unhandled events and logs them using the dynamic variable `*received-event*` for context.

#### Running the FSM

After setup, the FSM processes events, transitioning as defined. This setup ensures responsive and structured event-driven state management.

Incorporating timeout controls and a comprehensive fallback for unhandled events, this FSM elegantly manages complex state logic with powerful macro functionalities.

For more examples have a look at the [tests](tests/fsm-test.lisp).

### Router

A `Router` is a facade over a set of actors. Routers are either created with a set of actors using the default constructor `router:make-router` or actors can be added later.

Routers implement part of the actor protocol, so it allows to use `tell`, `ask-s` or `ask` which it forwards to a 'routee' (one of the actors of a router) by passing all of the given parameters. The routee is chosen by applying a `strategy`. The built-in default strategy a routee is chosen randomly.

The `strategy` can be configured when creating a router using the constructors `&key` parameter `:strategy`. The `strategy` is just a function that takes the number of routees and returns a routee index to be chosen for the next operation.

Currently available strategies: `:random` and`:round-robin`.

Custom strategies can be implemented.

### Dispatchers

#### :shared

A `:shared` dispatcher is a facility that is set up in the `actor-system`. It consists of a configurable pool of 'dispatcher workers' (which are in fact actors). Those dispatcher workers execute the message handling in behalf of the actor and with the actors message handling code. This is protected by a lock so that ever only one dispatcher will run code on an actor. This is to ensure protection from data race conditions of the state data of the actor (or other slots of the actor).

Using this dispatcher allows to create a large number of actors. The actors as such are generally very cheap.

<img alt="" src="./docs/disp_shared.png" width="700"/>
<img alt="" src="disp_shared.png" width="700"/>

#### :pinned

The `:pinned` dispatcher is represented by just a thread that operates on the actors message queue. It handles one message after another with the actors message handling code. This also ensures protection from data race conditions of the state of the actor.

This variant is slightly faster (see below) but requires one thread per actor.

<img alt="" src="./docs/disp_pinned.png" width="700"/>
<img alt="" src="disp_pinned.png" width="700"/>


#### custom dispatcher

It is possible to create additional dispatcher of type `:shared`. A name can be freely chosen, but by convention it should be a global symbol, i.e. `:my-dispatcher`.

When creating actors using `act:actor-of`, or when using the `tasks` API it is possible to specify the dispatcher (via the 'dispatcher-id' i.e. `:my-dispatcher`) that should handle the actor, agent, or task messages.

A custom dispatcher is in particular useful when using `tasks` for longer running operations. Longer running operations should not be used for the `:shared` dispatcher because it is, by default, responsible for the message handling of most actors.

### Eventstream

The eventstream allows messages (or events) to be posted on the eventstream in a fire-and-forget kind of way. Actors can subscribe to the eventstream if they want to get notified for particular messages or any message posted to the event stream.  
This allows to create event-based systems.

Here is a simple example:

```elisp
(defparameter *sys* (asys:make-actor-system))

(ac:actor-of *sys* :name "listener"
  :init (lambda (self)
          (ev:subscribe self self 'string))
  :receive (lambda (msg)
             (cond
               ((string= "my-message" msg)
                (format t "received event: ~a~%" msg)))))

(ev:publish *sys* "my-message")
```

This subscribes to all `'string` based events and just prints the message when received.  
The subscription here is done using the `:init` hook of the actor. The `ev:subscribe` function requires to specify the eventstream as first argument. But there are different variants of the generic function defined which allows to specify an actor directly. The eventstream is retrieve from the actor through its actor-context.

```
received event: my-message
```

See the API [documentation](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.EVENTSTREAM:EVENTSTREAM%20CLASS) for more details.

### Tasks

'tasks' is a convenience package that makes dealing with asynchronous and concurrent operations very easy.

Here is a simple example:

```elisp
(defparameter *sys* (make-actor-system))

(with-context (*sys*)
  
  // run something without requiring a feedback
  (task-start (lambda () (do-lengthy-IO))
  
  // run asynchronous - with await
  (let ((task (task-async (lambda () (do-a-task)))))
    // do some other stuff
    // eventually we need the task result
    (+ (task-await task) 5))
    
  // run asynchronous with completion-handler (continuation)
  (task-async (lambda () (some-bigger-computation))
              :on-complete-fun
              (lambda (result)
                (do-something-with result)))

  // concurrently map over the given list
  (->> 
    '(1 2 3 4 5)
    (task-async-stream #'1+)
    (reduce #'+)))

=> 20 (5 bits, #x14, #o24, #b10100)

```

All functions available in 'tasks' package require to be wrapped in a `with-context` macro. This macro removes the necessity of an additional argument to each of the functions which is instead supplied by the macro.

What happens in the example above is that the list `'(1 2 3 4 5)` is passed to `task-async-stream`. `task-async-stream` then spawns a 'task' for each element of the list and applies the given function (here `1+`) on each list element. The function though is executed by a worker of the actor-systems `:shared` dispatcher. `task-async-stream` then also collects the result of all workers. In the last step (`reduce`) the sum of the elements of the result list are calculated.

It is possible to specify a second argument to the `with-context` macro to specify the dispatcher that should be used for the tasks.  
The concurrency here depends on the number of dispatcher workers.

As alternative, or in special circumstances, it is possible to setf `*task-context*` and/or `*task-dispatcher*` special variables which allows to use **tasks** without `with-context` macro.

Be also aware that the `:shared` dispatcher should not run long running operations as it blocks a message processing thread. Create a custom dispatcher to use for `tasks` when you plan to operate longer running operations.

See the API [documentation](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.TASKS:WITH-CONTEXT%20MGL-PAX:MACRO) for more details.

### Immutability

Some words on immutability. Actor states don't need to be immutable data structures. Sento does _not_ make copies of the actor states. The user is responsible for the actor state and to motate the actor state only within 'receive' function.

### Logging

Sento does its own logging using different log levels from 'trace' to 'error' using log4cl. If you wish to also use log4cl in your application but find that Sento is too noisy in debug and trace logging you can change the log level for the 'sento package only by:

```
(log:config '(sento) :warn)
```

This will tell log4cl to do any logging for sento in warn level.

### Benchmarks

Hardware specs (M1)):

-   Mac M1 Ultra, 64 GB RAM

![](./docs/perf-M1Ultra.png)
![](perf-M1Ultra.png)

Hardware specs (x86-64):

-   iMac Pro (2017), 8 Core Xeon, 32 GB RAM

![](./docs/perf-x86_64.png)
![](perf-x86_64.png)


**All**

The benchmark was created by having 8 threads throwing each 125k (1M altogether) messages at 1 actor. The timing was taken for when the actor did finish processing those 1M messages. The messages were sent by either all `tell`, `ask-s`, or `ask` to an actor whose message-box worked using a single thread (`:pinned`) or a dispatched message queue (`:shared` / `dispatched`) with 8 workers.

Of course a `tell` is in most cases the fastest one, because it's the least resource intensive and there is no place that is blocking in this workflow.

**SBCL (v2.4.9)**

SBCL is very fast, but problematic in the synchronous ask case using :shared dispatcher.

**LispWorks (8.0.1)**

LispWorks is fast overall. Not as fast as SBCL. But it seems the GC is more robust, in particular on the `dispatched - ask`.

**CCL (v1.13)**

Unfortunately CCL doesn't work natively on M1 Apple CPU.

**ABCL (1.9)**

The pleasant surprise was ABCL. While not being the fastest it is very robust.

**Clasp 2.6.0**

Very slow. Used default settings, as also for the other tests.
Maybe something can be tweaked?

### Migration guide for moving from Sento 2 to Sento 3

- the receive function is now 1-arity. It only takes the a message parameter.
Previous 'self' and 'state' parameters are now accessible via `*self*` and `*state*`. The same applies to `become` function.

- the return value of 'receive' function has always been a bit of an obstacle. So now it is ignored for `tell` and `ask`. In both cases a `reply` function can be used to reply to a sender. `reply` implicitly uses `*sender*` but can be overriden (see 'long running and asynchronous operations in receive'). The 'receive' function return value is still relevant for `ask-s`, but now it doesn't need to be a `cons`. Whatever is returned is received by `ask-s`.

- 'utils' package has been split to 'timeutils' for i.e. ask-timeout condition, and 'miscutils' for i.e. filter function.

### Version history

**Version 3.4.0 (5.10.2024):** Finalized finite-state-machine (FSM) and documentation.

**Version 3.3.3 (1.10.2024):** Bug fix for actor with dispatcher mailbox where the order of processing messages wasn't honoured.

**Version 3.3.2 (14.8.2024):** Primarily documentation changes in regards to `future`

**Version 3.3.0 (1.7.2024):** See: [Changelog](https://github.com/mdbergmann/cl-gserver/compare/3.2.1...3.3.0)

**Version 3.2.0 (13.2.2024):** Message-box queue changes. SBCL now uses a separate fast CAS based queue coming as a contrib package. The other impls use a faster queue by default but still with locking. New benchmarks.

**Version 3.1.0 (14.1.2024):** Added scheduler facility to actor-system that allows to schedule functions one or recurring. See API documentation for more info.

**Version 3.0.4 (10.7.2023):** Allow additional initialization arguments be passed to actor. Wheel-time now knows CANCEL function. Partial fix for clasp (2.3.0).

**Version 3.0.3 (1.4.2023):** Minor implementation changes regarding pre-start and after-stop.

**Version 3.0.2 (6.4.2023):** Fix for actor stopping with 'wait'.

**Version 3.0.0 (1.2.2023):** New major version. See migration guide if you have are migrating from version 2.

**Version 2.2.0 (27.12.2022):** Added stashing and unstashing of messages.

**Version 2.1.0 (17.11.2022):** Reworked the `future` package. Nicer syntax and futures can now be mapped.

**Version 2.0.0 (16.8.2022):** Rename to "Sento". Incompatible change due to package names and system have changed.

**Version 1.12.2 (29.5.2022):** Removed the logging abstraction again. Less code to maintain. log4cl is featureful enough for users to either use it, or use something else in the applications that are based on sento.

**Version 1.12.1 (25.5.2022):** Shutdown and stop of actor, actor context and actor system can now wait for a full shutdown/stop of all actors to really have a clean system shutdown.

**Version 1.12.0 (26.2.2022):** Refactored and cleaned up the available `actor-of` facilities. There is now only one. If you used the macro before, you may have to adapt slightly.

**Version 1.11.1 (25.2.2022):** Minor additions to `actor-of` macro to allow specifying a `destroy` function.

**Version 1.11.0 (16.1.2022):** Changes to `AC:FIND-ACTORS`. Breaking API change. See API documentation for details.

**Version 1.10.0:** Logging abstraction. Use your own logging facility. sento doesn't lock you in but provides support for log4cl. Support for other logging facilities can be easily added so that the logging of sento  will use your chosen logging library. See below for more details.

**Version 1.9.0:** Use wheel timer for `ask` timeouts.

**Version 1.8.2:** atomic add/remove of actors in actor-context.

**Version 1.8.0:** hash-agent interface changes. Added array-agent.

**Version 1.7.6:** Added cl:hash-table based agent with similar API interface.

**Version 1.7.5:** Allow agent to specify the dispatcher to be used.

**Version 1.7.4:** more convenience additions for task-async (completion-handler)

**Version 1.7.3:** cleaned up dependencies. Now sento works on SBCL, CCL, LispWorks, Allegro and ABCL

**Version 1.7.2:** allowing to choose the dispatcher strategy via configuration

**Version 1.7.1:** added possibility to create additional and custom dispatchers. I.e. to be used with `tasks`.

**Version 1.7.0:** added tasks abstraction facility to more easily deal with asynchronous and concurrent operations.

**Version 1.6.0:** added eventstream facility for building event based systems. Plus documentation improvements.

**Version 1.5.0:** added configuration structure. actor-system can now be created with a configuration. More configuration options to come.

**Version 1.4.1:** changed documentation to the excellent [mgl-pax](https://github.com/melisgl/mgl-pax)

**Version 1.4:** convenience macro for creating actor. See below for more details

**Version 1.3.1:** round-robin strategy for router

**Version 1.3:** agents can be created in actor-system

**Version 1.2:** introduces a breaking change

`ask` has been renamed to `ask-s`.

`async-ask` has been renamed to `ask`.

The proposed default way to query for a result from another actor should
be an asynchronous `ask`. `ask-s` (synchronous) is
of course still possible.

**Version 1.0** of `sento` library comes with quite a
few new features (compared to the previous 0.x versions). 
One of the major new features is that an actor is not
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
