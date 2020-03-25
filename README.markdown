# Cl-GServer

GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.  
Encapsulating state also means that it gives a safe facility to update state within a multi-threaded environment without having you to worry about maintaining access to state.

State can be changed and maintained by calling into the server via 'call' or 'cast'.  
Where 'call' is synchronous and waits for a result, 'cast' is asynchronous and responds just with `t`.
For each 'call' and 'cast' handlers must be implemented by subclasses.

GServer runs it's own thread that handles the incoming messages and maintains the state.
In that regard message handling should be quick. Long operations should be delegated to elsewhere.


In it's functionality regarding state it is also not unsimilar:
- to Clojure's [Agent](https://clojure.org/reference/agents) 
- or [cl-actors](https://github.com/naveensundarg/Common-Lisp-Actors).


## Usage

### GServer

#### Creating a custom gserver

First `:use :cl-gserver`.

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

#### Performance considerations

As in this simple test 100_000 messages were processed in 660ms.

```
CL-GSERVER> (log:config :warn)
CL-GSERVER> (time (iter:iter (iter:repeat 100000)
                    (call *my-server* :foo)))

Evaluation took:
  0.660 seconds of real time
  1.115068 seconds of total run time (0.899642 user, 0.215426 system)
  [ Run times consist of 0.013 seconds GC time, and 1.103 seconds non-GC time. ]
  168.94% CPU
  2,107,197,625 processor cycles
  80,363,168 bytes consed
```

## Agent

### Usage

An `Agent` is a specialized `GServer`. It is meant primarily for maintaining state and comes with some conveniences to do that.

To use an Agent import `cl-gserver.agent` package.

There is no need to subclass an Agent. An Agent provides three functions to use it.

- `make-agent` creates a new agent
- `agent-get` retrieves the current value of the agent
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


## Actor / Simple-Actor

### Usage

Actors are another abstraction, or use-case of a `GServer` (GenServer). However, actors don't exist in Erlang. Actors are pretty much a `GServer` only that they provider only one method `receive` to handle both `send` (which is like `cast`) and `ask` (which is like `call`).  
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
                                     :receive-fun
                                     (lambda (self msg state)
                                       (trivia:match msg
                                         ((cons :ping sender) (progn
                                                                (log:info "ping from: " sender)
                                                                (sleep 1)
                                                                (when (< state 5)
                                                                  (act:send sender (cons :pong self))
                                                                  (cons nil (1+ state)))))))))
```

The convenience `make-actor` function allows you to create a simple actor by specifying the `receive-fun` inline.
Of course you may create a `defun` for the receive and specify it for `:receive-fun` like `#'my-receive`.  
The important thing, it must accept three parameters. That is:

- `self`: for the self instance
- `message`: the received message
- `current-state`: for the current state of the actor.

The `receive-fun` also must return a `cons` equal to the `GServer` with value for reply and new state.

Now let's create the pong actor:

```lisp
(defparameter *pong* (act:make-actor "pong" 
                                     :receive-fun
                                     (lambda (self msg state)
                                     (trivia:match msg
                                       ((cons :pong sender) (progn
                                                              (log:info "pong from: " sender)
                                                              (sleep 1)
                                                              (act:send sender (cons :ping self))
                                                              (cons nil nil)))))))
```

Now we have two actors which can play ping pong.  
We trigger it my sending a `:ping` to the `*ping*` actor but we also specify the `*pong*` actor as sender.

`(act:send *ping* (cons :ping *pong*))`

As can be seen on the `*ping*` actor definition, it will update it's state by incrementing the received pings. Once they are >= 5 it will stop sending a pong.

