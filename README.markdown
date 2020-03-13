# Cl-GServer

GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.  
Encapsulating state also means that it gives a safe facility to update state within a multi-threaded environment without having you to worry about maintaining access to state.

State can be changed and maintained by calling into the server via 'call' or 'cast'.  
Where 'call' is synchronous and waits for a result, 'cast' is asynchronous and responds just with `t`.
For each 'call' and 'cast' handlers must be implemented by subclasses.

GServer runs one it's own thread that handles the incoming messages and eventually maintaining the state.
In that regard message handling should be quick. Long operations should be delegated to elsewhere.

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

### Performance considerations

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


## WIP/TODO

- Make convenience macros for creating a server.
- implement Agent that works with lamdas.
