# Cl-GServer

GServer is an Erlang inspired GenServer.
It is meant to encapsulate state, but also to execute async operations.

State can be changed and maintained by calling into the server via 'call' or 'cast'.  
Where 'call' is synchronous and waits for a result, 'cast' is asynchronous and responds just with `t`.
For each 'call' and 'cast' handlers must be implemented by subclasses.

GServer runs at least one thread that operates on a queue to update the GServer state.
Calls (or casts) are dispatched using either a GServer internal threadpool, or a global threadpool.

## Usage

### Dispatcher thread pool

First `:use :cl-gserver`.

As said above, the GServer maintains a state synchronization queue.  
`call`s and `cast`s are dispatched in a different threadpool. GServer supports creating a per server threadpool, or a global threadpool.

To initialize a global threadpool you should first call:

```lisp
(init-dispatcher-threadpool 4)
```

You can choose when creating a GServer which threadpool to use.

See below for performance considerations for the number of workers in a pool.


### Creating a custom gserver

Let's create a simple stack gserver:

First create a new subclass of `gserver`:

```lisp
(defclass stack-server (gserver) ())
```

Then implement `handle-call` method which is used to pop or get values since `call`ing a `gserver` waits for result.
Both `handle-call` and `handle-cast` provide three parameters. That is the 'server' instance, the 'message' that was sent, and the 'current-state' of the `gserver`:

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
You are free to implement the handlers however you like as long as the return conventions are met.

The convention of `handle-call` is to always return a `cons` where the `car` value is to value to be returned and the new state value as `cdr`.

So `:pop` takes the current `car` of the backing list which will be returned to the caller and `cdr` of the current state will become the new state.

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
We still have to return a `cons`. However, the `car` of the cons is kind of irrelevant, because it's not returned to the caller. The `cdr` is important as it will get the new state.

Disclaimer: this is a completely naive implementaion of a stack just using a list structure.


#### `call`ing and `cast`ing

Now we can make a new server instance with a predefined stack of one entry: 5:

```lisp
(defparameter *stack-server* (make-instance 'stack-server :state '(5)))
```

When `:dispatch-workers` initarg is specified with a value > 0, then this instance will create a dedicated threadpool with the specified numbers of workers. If the iniarg is ommited or specified with 0 then a global threadpool will be used that has to be initialized using `init-dispatcher-threadpool`.


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

### Performance coonsiderations

Tests show that if the handlers are usd to do quick operations, then a lower number of workers will be faster.

```
--- GLOBAL dispatcher ---

CL-GSERVER> (log:config :warn)
CL-GSERVER> (init-dispatcher-threadpool 1)
CL-GSERVER> (time (iter:iter (iter:repeat 10000)
                    (call *my-server* :foo)))
Evaluation took:
  0.231 seconds of real time
  0.302233 seconds of total run time (0.222568 user, 0.079665 system)
  130.74% CPU
  735,383,473 processor cycles
  11,500,384 bytes consed


CL-GSERVER> (init-dispatcher-threadpool 8)
CL-GSERVER> (time (iter:iter (iter:repeat 10000)
                    (call *my-server* :foo)))
Evaluation took:
  0.462 seconds of real time
  0.302238 seconds of total run time (0.221863 user, 0.080375 system)
  65.37% CPU
  1,474,141,478 processor cycles
  11,500,096 bytes consed
```

However, you should have more workers if the handlers do longer operations, otherwise the messages will queue up.

For some reason that I didn't research yet, when using a dedicated threadpool for the gserver, then message handling is slower:

```
--- INTERNAL kernel ---

CL-GSERVER> (defparameter *my-server* (make-instance 'my-server :state 0 :dispatch-workers 1))
*MY-SERVER*
CL-GSERVER> (time (iter:iter (iter:repeat 10000)
                    (call *my-server* :foo)))
Evaluation took:
  0.590 seconds of real time
  0.271001 seconds of total run time (0.194842 user, 0.076159 system)
  45.93% CPU
  1,883,089,220 processor cycles
  11,499,968 bytes consed


CL-GSERVER> (defparameter *my-server* (make-instance 'my-server :state 0 :dispatch-workers 2))
*MY-SERVER*
CL-GSERVER> (time (iter:iter (iter:repeat 10000)
                    (call *my-server* :foo)))
Evaluation took:
  1.390 seconds of real time
  0.449266 seconds of total run time (0.356600 user, 0.092666 system)
  32.30% CPU
  4,437,370,508 processor cycles
  11,468,128 bytes consed
```


## WIP/TODO

- Make convenience macros for creating a server.
- implement Agent that works with lamdas.
