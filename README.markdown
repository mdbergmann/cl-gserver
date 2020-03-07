# Cl-GServer

GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.

State can be changed and maintained by calling into the server via 'call' or 'cast'.  
Where 'call' is waiting for a result and 'cast' does not.
For each 'call' and 'cast' handlers must be implemented by subclasses.

A GServer runs a 'mailbox'-queue in it's own thread so that the order is which messages arrive at the GServer are handled are preserved.

## Usage

#### Dispatcher thread pool

It's quite simple. First `:use :cl-gserver`.

Initialize a dispatcher threadpool. The dispatcher threadpool should be used to execute long running tasks in the implemented handlers `handle-call` and `handle-cast`.
Short lived I/O tasks can use more threads in the pool, if the threads have more CPU intensive work to do then the number of threads should match the number of CPU cores on your hardware.

```lisp
(init-dispatcher-threadpool 4)
```

#### Creating a custom gserver

Let's create a stack gserver:

First create a new subclass:

```lisp
(defclass stack-server (gserver) ())
```

Then implement `handle-call` method which is used to pop or get values since `call`ing a gserver waits for result.
Both `handle-call` and `handle-cast` provide three parameters. That is the 'server' instance, the 'message' that was sent, and the 'current-state' of the gserver:

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

This implements two message handlers using pattern matching of `trivia` library.  
The convention of `handle-call` is to always return a `cons` of the `car` value to be returned and the new state value as `cdr`.

So `:pop` takes the current `car` of the backing list which will be returned to the caller and `cdr` of the current state will get the new state.

Now we also want to push values. This will be done by `cast`ing to the server.

```lisp
(defmethod handle-cast ((server stack-server) message current-state)
  (log:debug "current-state: " current-state)
  (match message
    ((cons :push value)
     (let ((new-state (append current-state (list value))))
       (cons new-state new-state)))))
```

`cast` is asynchronous and doesn't wait for a result. So we use it to push values to the stack.
We still have to returns a `cons`. However, the `car` of the cons is kind of irrelevant, because it's not returned to the caller. The `cdr` is important as it will get the new state.

Disclaimer: this is a completely naive implementaion of a stack just using a list structure.

Attention: `handle-call` and `handle-cast` are executed by 'mailbox' handling thread of the GServer. For long running tasks make sure you wrap your code into a `lparallel:future`.

#### `call`ing and `cast`ing

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

## WIP/TODO

- Make convenience macros for creating a server.
- implement Agent that works with lamdas.
