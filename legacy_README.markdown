# Cl-GServer

GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.  
Encapsulating state also means that it gives a safe facility to update state within a multi-threaded environment without having you to worry about maintaining access to state.

State can be changed and maintained by calling into the server via 'call', 'async-call' or 'cast'.  
Where 'call' is synchronous and waits for a result. 'async-call' is asynchronous and returns a 'future' object representing a future computation of the 'call'. The 'cast' is also asynchronous and responds just with `t`.
For 'call', 'async-call' and 'cast' message handlers must be implemented by subclasses.

GServer runs it's own thread that handles the incoming messages and maintains the state.
In that regard message handling should be quick. Long operations should be delegated to elsewhere.


In it's functionality regarding state it is also not unsimilar:  
- to Clojure's [Agent](https://clojure.org/reference/agents)  
- Akka Actors library for JVM  
- [cl-actors](https://github.com/naveensundarg/Common-Lisp-Actors)

This library also provides an `Actor` type and an `Agent` type. Both based on `Gserver`.


## Usage

### GServer

#### Creating a custom gserver

First `:use :cl-gserver.actor-cell`.

Let's create a simple stack gserver:

First create a new subclass of `gserver`:

```lisp
(defclass stack-server (gserver) ())
```

##### Synchronous `call`

Then implement `handle-call` method which is used to pop or get values since `call`ing a `gserver` waits for result.
Both `handle-call` and `handle-cast` provide three parameters. That is the 'server' instance ('self' if you want), 
the 'message' that was sent, and the 'current-state' of the `gserver`:

```lisp
(defmethod handle-call ((server stack-server) message current-state)
  (log:debug "current-state: " current-state)
  (match message
    (:pop
     (cons
      (car current-state)
      (cdr current-state)))
    (:get
     (cons current-state current-state))))
```

This implements two message handlers using pattern matching with help of `trivia` library.  
You are free to implement the handlers however you like as long as the return conventions are met.
An error is raised if no `cons` is returned, in which case the server responds with `(cons :handler-error "<error-message>")` to the `call`.

The convention of `handle-call` is to always return a `cons` where the `car` value is to be returned and the new state value is `cdr`.

So `:pop` in the examnple takes the current `car` of the backing list which will be returned to the caller and `cdr` of the current state will become the new state.

##### Asynchronous `async-call`

Generally all that applies to `call` also applies to `async-call`. The only difference is that `async-call` returns immediately with a `future` object. In an asynchronous environment you'd want to install a completed handler on the future like this:

```
(on-completed 
  (async-call *the-gserver* <the-message>)
  (lambda (promise)
    ;; do something with the promise))
```

The `promise` of the `lambda` is the result of the `async-call`. You can also probe for completeness of the future with `(complete-p <future>)` or get the result with `(get-result <future>)`. Be aware though that `get-result` returns `not-ready` if the future is not completed.


##### Asynchronous `cast`

Now we also want to push values. This will be done by `cast`ing to the server.

```lisp
(defmethod handle-cast ((server stack-server) message current-state)
  (log:debug "current-state: " current-state)
  (match message
    ((cons :push value)
     (let ((new-state (append current-state (list value))))
       (cons new-state new-state)))))
```

`cast` is asynchronous and just responds with `t`. So we can use it to push values to the stack.
We still have to return a `cons`. However, the `car` of the cons is kind of irrelevant, because it's not returned to the caller. The `cdr` is important as it will get the new state.

Disclaimer: this is a completely naive implementaion of a stack just using a cons list.
Disclaimer2: since `cast` is asynchronous the push might not yet has updated the gserver state when you do a pop immediately after.
So this is not the best example for a `cast`.


##### Make instance of stack-server

Now we can make a new server instance with a predefined stack of one entry: 5:

```lisp
(defparameter *stack-server* (make-instance 'stack-server :state '(5)))
```

Let's push new values:

```lisp
(cast *stack-server* (cons :push 4))
(cast *stack-server* (cons :push 3))
(cast *stack-server* (cons :push 2))
```

When we check the state, we get:

```lisp
(call *stack-server* :get)
=> returns '(5 4 3 2)
```

We can also pop the stack: 

```lisp
(call *stack-server* :pop)
=> returns 5
(call *stack-server* :pop)
=> returns 4
(call *stack-server* :pop)
=> returns 3
(call *stack-server* :pop)
=> returns 2
```

## Actor / Simple-Actor

### Usage

Actors are another abstraction, or use-case of a `GServer` (GenServer). However, actors don't exist in Erlang. Actors are pretty much a `GServer` only that they provider only one method `receive` to handle both `tell` (which is like `cast`), `ask` (which is like `call`) and `async-ask` (which is like `async-call`).  
The `cons` return of `receive` is also a convention.

To use actors import the `cl-gserver.actor` package.

You have two choices to implement your actors.

##### 1. Make your own subclass of `actor` and implement `defmethod receive ...` (see the actor-test.lisp for details).

##### 2. Use the `simple-actor` convenience implementation

Let me make a simple ping-pong example here:

(I've added a local package name as `act` for `cl-gserver.actor`).

First create a ping actor:

```lisp
(defparameter *ping* (act:make-actor "ping" 
                                     :state 0 
                                     :receive
                                     (lambda (self msg state)
                                       (trivia:match msg
                                         ((cons :ping sender) (progn
                                                                (log:info "ping from: " sender)
                                                                (sleep 1)
                                                                (when (< state 5)
                                                                  (act:tell sender (cons :pong self))
                                                                  (cons nil (1+ state)))))))))
```

The convenience `make-actor` function allows you to create a simple actor by specifying the `behavior` inline.
Of course you may create a `defun` for the receive and specify it for `:receive` like `#'my-receive`.  
The important thing, it must accept three parameters. That is:

- `self`: for the self instance
- `message`: the received message
- `current-state`: for the current state of the actor.

The `behavior` also must return a `cons` equal to the `GServer` with value for reply and new state.

Now let's create the pong actor:

```lisp
(defparameter *pong* (act:make-actor "pong" 
                                     :receive
                                     (lambda (self msg state)
                                     (trivia:match msg
                                       ((cons :pong sender) (progn
                                                              (log:info "pong from: " sender)
                                                              (sleep 1)
                                                              (act:tell sender (cons :ping self))
                                                              (cons nil nil)))))))
```

Now we have two actors which can play ping pong.  
We trigger it my telling a `:ping` to the `*ping*` actor but we also specify the `*pong*` actor as sender.

`(act:tell *ping* (cons :ping *pong*))`

As can be seen on the `*ping*` actor definition, it will update it's state by incrementing the received pings. Once they are >= 5 it will stop telling a pong.

## Agent

### Usage

An `Agent` is a specialized `GServer`. It is meant primarily for maintaining state and comes with some conveniences to do that.

To use an Agent import `cl-gserver.agent` package.

There is no need to subclass an Agent. An Agent provides three functions to use it.

- `make-agent` creates a new agent
- `agent-get` retrieves the current state of the agent. This directly delivers the state of the agent for performance reasons. There is no message handling involved.
- `agent-update` updates the state of the agent

All three take a lambda.  
The lambda for `make-agent` does not take a parameter. It should return the initial state of the agent.  
`agent-get` and `agent-update` both take a lambda that must support one parameter. This parameter represents the current state of the agent.

Let's make a simple example:

First create an agent with an initial state of `0`.

```lisp
(defparameter *my-agent* (make-agent (lambda () 0)))
```

Now update the state several times (`agent-update` is asynchronous and returns `t` immediately):

```lisp
(agent-update *my-agent* (lambda (state) (1+ state)))
```

Finally get the state:

```lisp
(agent-get *my-agent* #'identity)
```

This `agent-get` just uses the `identity` function to return the state as is.

So this simple agent represents a counter.

It is important to note that the retrieves state, i.e. with `identity` should not be modified outside the agent.

#### Wrapping an agent

While you can use the agent as in the example above it is usually advised to wrap an agent behind a more simple facade that doesn't work with lambdas.

For example could a facade for the counter above look like this:

```lisp
(defvar *counter-agent* nil)

(defun init-agent (initial-value)
  (setf *counter-agent* (make-agent (lambda () initial-value))))

(defun increment () (agent-update *counter-agent* #'1+))
(defun decrement () (agent-update *counter-agent* #'1-))
(defun counter-value () (agent-get *counter-agent* #'identity))
```

Alternatively, one can wrap an agent inside a class and provide methods for simplified access to it.


## Cleaning up resources

A GServer/Actor/Agent can be stopped which will stop the message processing thread.  
For GServer and Actor you just tell a `:stop` message and it will respond with `:stopped`.
The `:stop` message is queued the same way as ordinary messages.

To stop an Agent you need to call `agent-stop` function. It will also respond with `:stopped`.

## Performance considerations

The test results here were done on an 8 core Xeon system with SBCL.  
Number of messages: 8 million with 8 threads each telling 1 million messages.

The message-box is a single [Bordeaux-Threads](https://github.com/sionescu/bordeaux-threads) thread which can operate on:

### An unbounded queue

The queue used here is the cons-queue of [lparallel](https://github.com/lmj/lparallel) which has a good performance.

8 million messages could be queezed through this message-box in ~7 seconds.

**SBCL:**

```
Evaluation took:
  6.976 seconds of real time
  51.041781 seconds of total run time (50.774304 user, 0.267477 system)
  [ Run times consist of 0.725 seconds GC time, and 50.317 seconds non-GC time. ]
  731.68% CPU
  22,268,009,162 processor cycles
  384,150,656 bytes consed
  
Counter: 8000000
msg/s: 1142857.142857143
```

The above result is for messages that don't require a feedback. `ask`, or `call` messages whcih must deliver the result to the caller are much slower.

Message handling with result delivery:

```
Evaluation took:
  18.181 seconds of real time
  107.494489 seconds of total run time (30.038678 user, 77.455811 system)
  [ Run times consist of 0.218 seconds GC time, and 107.277 seconds non-GC time. ]
  591.24% CPU
  58,034,749,891 processor cycles
  1,673,953,600 bytes consed
  
Counter: 8000000
```

This is because some additional locking and waiting must be performed to deliver the result to the caller when it was processed by the message queue.


**CCL 1.12 is quite a bit slower altogether than SBCL:**

```
took 25,432,484 microseconds (25.432484 seconds) to run.
      6,055,551 microseconds ( 6.055551 seconds, 23.81%) of which was spent in GC.
During that period, and with 16 available CPU cores,
     33,735,407 microseconds (33.735410 seconds) were spent in user mode
     96,117,573 microseconds (96.117580 seconds) were spent in system mode
 24,208 bytes of memory allocated.
 130,439 minor page faults, 1 major page faults, 0 swaps.
Counter: 8000000
```

**ECL 20.4.24:**

```
real time : 76.989 secs
run time  : 67.377 secs
gc count  : 3 times
consed    : 2952602000 bytes
Counter: 8000000
```

**ABCL 1.6.1:**

```
28.931 seconds real time
87105548 cons cells
Counter: 8000000
```

### Bounded queue

The bounded queue, based on the [cl-speedy-queue](https://github.com/zkat/cl-speedy-queue) with some locking around it is slightly faster on my test system. ~0.5s on average. Not much in fact.  
The bounded queue on really busy system has some memory resource advantages because it can be limited to value of max entries.
It starts back-pressuring when a 90% threshold is reached.


To choose between unbounded and bounded queue you specify `:max-queue-size <n>` as key argument when making an instance of `Gserver`, `Actor`, or `Agent`.
Specify 0 or `nil` for an unbounded queue and a value > 1 for a bounded queue. However, choose at least 10 entries.

### Comparison with Akka

A similar test with Akka on the JVM (Java 8) manages to process 8 million messages in ~4.5 seconds. Which is still a good portion faster than what CL can do.

## Alternatives

**STMX (transactional memory)**

I've also experimented with [stmx](https://github.com/cosmos72/stmx). This project has a fifo queue ('tfifo') that works with transactional memory.

But timing were by far not as good as with a locking queue. On SBCL, I could only try with 800_000 messages using 8 threads. The value of above, 8 million message couldn't be handled for some reason.
But the timing for 800_000 message shows that stmx on SBCL is at least 5 times slower that the locking cons-queue.

However, stmx, or transational memory in general is an alternative option to deal with state in multi-threaded environments.
