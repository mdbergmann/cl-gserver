(defpackage :sento.fsm
  (:use :cl :act)
  (:nicknames :fsm)
  (:export #:make-fsm
           #:fsm
           #:when-state
           #:on-event
           #:goto-state
           #:stay-on-state
           #:when-unhandled
           #:on-transition
           #:*state-data*
           #:*next-state-data*
           #:*received-event*
           #:*event-data*))

(in-package :sento.fsm)

(defclass fsm (actor)
  ((event-handling-fun :initarg :event-handling
                       :initform nil
                       :reader event-handling-fun)
   (timeouts :initform (make-hash-table :test #'eq)
             :reader timeouts))
  (:documentation "FSM Class

The FSM class represents a Finite State Machine, a mathematical model of computation that transitions between a finite number of states in response to external inputs."))

(defstruct fsm-state "Finite-state-machine state model."
  (state)
  (data))

;; public vars
(defvar *received-event* nil "Dynamically binds the received event (message).")
(defvar *event-data* nil "Dynamically binds event data when msg/event was sent with data (`cons')")
(defvar *state-data* nil "Dynamically binds the current state data.")
(defvar *next-state-data* nil
  "Dynamically binds the next state data (`on-transition').
Effectively same as `*event-data*' but should be used in different context.")

;; private vars
(defvar *current-state* nil "dynamically binds the current state")
(defvar *event-was-handled-by-goto* nil)
(defvar *event-was-handled-by-stay* nil)


(defmacro when-state ((state &key (test '#'eq) timeout-s) &body body)
  `(progn
     (when ,timeout-s
       (setf (gethash ,state (timeouts *self*)) ,timeout-s))
     (when (funcall ,test *current-state* ,state)
       (when ,timeout-s
         (log:debug "Registered timeout for state: ~a" ,state))
       ,@body)))

(defmacro on-event ((event &key (test '#'eq)) &body body)
  (let ((declares-timeout (find :state-timeout body)))
    `(when (or (funcall ,test *received-event* ,event)
               (and ,declares-timeout
                    (eq *received-event* :state-timeout)))
       ,@body)))

(defmacro goto-state (next-state &optional (data nil data-p))
  `(progn
     (setf (fsm-state-state *state*) ,next-state)
     (when ,data-p
       (setf (fsm-state-data *state*) ,data))
     (setf *event-was-handled-by-goto* t)))

(defmacro stay-on-state (&optional (data nil data-p))
  `(progn
     (when ,data-p
       (setf (fsm-state-data *state*) ,data))
     (setf *event-was-handled-by-stay* t)))

(defmacro when-unhandled ((event &key (test '#'eq)) &body body)
  `(unless (or *event-was-handled-by-stay* *event-was-handled-by-goto*)
     (when (funcall ,test *received-event* ,event)
       ,@body)))

(defmacro on-transition ((transition &key (test '#'eq)) &body body)
  `(when (and *event-was-handled-by-goto*
              (funcall ,test
                       *current-state*
                       (car ,transition))
              (funcall ,test
                       (fsm-state-state *state*)
                       (cdr ,transition)))
     ,@body))

(defun %receive (msg)
  (with-slots (event-handling-fun timeouts) *self*
    (when event-handling-fun
      (let* ((*current-state* (fsm-state-state *state*))
             (*state-data* (fsm-state-data *state*))
             (*received-event* (if (consp msg)
                                   (car msg)
                                   msg))
             (*event-data* (if (consp msg)
                               (cdr msg)
                               nil))
             (*next-state-data* *event-data*)
             (*event-was-handled-by-stay* nil)
             (*event-was-handled-by-goto* nil))
        (log:debug "Current state: ~a, event: ~a" *current-state* msg)
        (log:debug "Current data: ~a, event-data: ~a, next-data: ~a"
                   *state-data* *event-data* *next-state-data*)
        (handler-case
            (funcall event-handling-fun)
          (error (c)
            (log:warn "Error in event handler: ~a" c)))
        (log:debug "Last state: ~a, new state: ~a" *current-state* (fsm-state-state *state*))
        (log:debug "New data: ~a" *state-data*)

        (%setup-timeouts timeouts)))))

(defun %setup-timeouts (timeouts)
  (let* ((new-state (fsm-state-state *state*))
         (timeout (gethash new-state timeouts)))
    (when (and timeout
               *event-was-handled-by-goto*)
      (log:debug "State change with timeout. Scheduling timeout for state: ~a" new-state)
      (let ((scheduler (asys:scheduler (ac:system *self*)))
            (self *self*))
        (wt:schedule-once scheduler
                          timeout
                          (lambda ()
                            (log:debug "Timeout elapsed, sending :state-timeout.")
                            (! self :state-timeout)))))))

(defun make-fsm (actor-context &key name start-with event-handling
                                 (type 'fsm)
                                 (dispatcher-id :shared))
  (check-type name string)
  (check-type start-with cons)
  (check-type event-handling (or null function))
  (ac:actor-of actor-context
               :type type
               :name name
               :receive (lambda (msg)
                          (%receive msg))
               :state (make-fsm-state :state (car start-with)
                                      :data (cdr start-with))
               :event-handling event-handling
               :dispatcher dispatcher-id))

;; -------------------------------------------
;; ----------- docstrings --------------------
;; -------------------------------------------

(setf (documentation 'make-fsm 'function)
      "`make-fsm` Function

Creates a finite state machine (FSM) within the given `actor-context`.

**Parameters**

- `actor-context`: Can be an actor, an actor-context (class), or an actor-system in which this FSM is created.
  
- `name`: A string representing the name of the FSM. Must be a string.
  
- `start-with`: A cons cell where the car is the initial state and the cdr is the initial data for the FSM. Must be a cons.

- `event-handling`: An optional function for handling events. It can be `nil` if not provided. Must be either a function or `nil`. If omitted, the FSM will effectively do nothing. The function body should be constructed using the provided FSM-related macros such as `on-event` and `on-transition`.

- `type`: The type of actor to create. Defaults to `'fsm`.

- `dispatcher-id`: Identifies the dispatcher for the FSM. Defaults to `:shared`.

**Description**

The `make-fsm` function initializes an FSM actor with a specified initial state and associated data. The FSM's behavior is defined by the `event-handling` function, which processes events if provided. This function should utilize the provided macros like `on-event` and `on-transition` to structure its body, enabling robust event handling and state transition management. Without this function, the FSM will not perform any actions.

This function configures the FSM within the given `actor-context`, ensuring it is properly set up according to the parameters specified through the `ac:actor-of` function.")

(setf (documentation 'when-state 'function)
      "`when-state` Macro

The `when-state` macro is used to conditionally execute a body of code when a defined condition on the FSM's (Finite State Machine) current state is met, with support for custom predicates and timeout management for nested `on-event` macros.

**Parameters**

- `state`: An arbitrary value or structure that represents the state to be checked against the FSM's current state. The usage and type should align with the `:test` function.

- `:test`: A predicate function used to evaluate if the FSM's current state matches the `state` argument. The default is `#'eq`, but can be customized with other functions or lambdas.

- `:timeout-s`: An optional timeout in seconds that is applied to `on-event` macro calls tagged with `:state-timeout` within the body.

- `body`: One or more forms, typically including `on-event` macro definitions, executed if the state condition is satisfied.

**Description**

`when-state` enables dynamic state-based programming within FSMs, allowing for flexible condition evaluation with customizable predicate functions. It also manages execution timeouts for actions specified within nested `on-event` calls. The `:timeout-s` parameter, when used with the `:state-timeout` tag, ensures operations are constrained to a specified period.

**Usage Example**

```lisp
(when-state ('active :test #'eq :timeout-s 10)
  (on-event ('start) :state-timeout
    (start-activity))
  (on-event ('stop)
    (stop-activity)))
```

In this example:
- `start-activity` is executed if the current FSM state is exactly `'active`, using `:test #'eq`, within the 10-second window specified by `:timeout-s` and tagged with `:state-timeout`.
- `stop-activity` runs upon receiving a `stop` event, without timeout constraints.

**Notes**

- Adjust the `:test` predicate to suit the structure and type of your `state` input as needed.
- `:timeout-s` specifies a duration within which tagged events should occur, integrating with the `on-event` macro.
- Ensure that each `on-event` is properly enclosed in parentheses, reflecting its syntax.

Use the appropriate predicate function to match the `state` argument's format, ensuring meaningful and effective FSM operations.")

(setf (documentation 'on-event 'function)
      "`on-event` Macro

The `on-event` macro defines actions to be executed when specific events occur within an FSM (Finite State Machine). It is often used within the `when-state` macro to enable conditional execution based on state and optional timeout constraints.

**Parameters**

- `event`: The event name or identifier to be monitored. This argument specifies which event should trigger the execution of the provided body.

- `:state-timeout`: A tag indicating that the execution of this event's actions is subject to the `:timeout-s` specified in a surrounding `when-state` macro.

- `body`: One or more expressions representing the actions to be executed when the specified event occurs.

**Description**

The `on-event` macro facilitates event-driven actions within FSMs. When used within a `when-state` block and tagged with `:state-timeout`, it ensures that the actions are executed within a specified time period after the event occurs, contingent on the current state of the FSM.

**Usage Example**

```lisp
(when-state ('active :test #'eq :timeout-s 10)
  (on-event 'start :state-timeout
    (start-activity))
  (on-event 'stop
    (stop-activity)))
```

In this example:
- The `start-activity` action is executed when the `start` event occurs, provided the FSM is in the `active` state within the 10-second timeout duration.
- The `stop-activity` is triggered by a `stop` event without timeout constraints.

**Notes**

- `:state-timeout` indicates that the timeout from `when-state` should apply to this event's execution.
- Ensure the event detection mechanism within your FSM can recognize and handle the specified `event` argument.

Use `on-event` macros within `when-state` to manage event responses systematically and within time constraints defined for specific states. Adjust the actions and logic as necessary for your FSM's behavior.")

(setf (documentation 'goto-state 'function)
      "`goto-state` Macro

The `goto-state` macro is used to transition the FSM (Finite State Machine) to a specified state, with optional data setting for the state model. This macro simplifies state management by providing a direct mechanism to switch states and update state-specific data.

**Parameters**

- `state`: The target state to which the FSM should transition. This can be a symbol or any other datatype representing the state, consistent with the FSM's state representation.

- `data`: An optional parameter to set the data associated with the new state. This allows for updating the state model with relevant information during the transition.

**Description**

The `goto-state` macro facilitates explicit state transitions and optionally updates the state model's data. It is typically invoked in response to specific conditions or events, allowing dynamic integration with other FSM constructs like `when-state` or `on-event`.

**Usage Example**

```lisp
(when-state ('idle :test #'eq :timeout-s 5)
  (on-event ('start)
    (goto-state 'active '(\"Session ID: 123\"))
    (perform-initialization)))

(when-state ('active :test #'eq)
  (on-event ('stop)
    (goto-state 'idle '(\"Clean exit\"))
    (perform-cleanup)))
```

In this example:
- The FSM transitions to the `active` state with associated data `\"Session ID: 123\"` upon receiving a `start` event while in the `idle` state, executing `perform-initialization`.
- It transitions back to the `idle` state with data `\"Clean exit\"` when the `stop` event occurs while the FSM is in the `active` state, executing `perform-cleanup`.

**Notes**

- Ensure that `state` is valid within the FSM's state space and that the transition complies with the FSM's logic and rules.
- The `data` parameter is optional, but when used, should be structured appropriately to fit the state model's requirements.

The `goto-state` macro, with its optional `data` capability, enhances flexibility and precision in managing FSM state transitions and data updates. Adjust the usage examples and structure to align with your FSM's specific needs and design.")

(setf (documentation 'stay-on-state 'function)
      "`stay-on-state` Macro

The `stay-on-state` macro is used to maintain the FSM (Finite State Machine) in its current state, with an option to update the state's associated data. This is useful for situations where the state needs to persist while its data is updated.

**Parameters**

- `data`: An optional parameter to update the data related to the current state. This allows for modifying the state model with new information without changing the state itself.

**Description**

The `stay-on-state` macro provides a way to remain in the current state of an FSM while updating any associated data. It can be used in reaction to specific events or conditions, maintaining state continuity while making data adjustments.

**Usage Example**

```lisp
(when-state ('processing :test #'eq)
  (on-event ('update)
    (stay-on-state '(\"Progress: 50%\"))
    (log-update)))

(when-state ('processing :test #'eq)
  (on-event ('complete)
    (goto-state 'completed '(\"Finished successfully\"))))
```

In this example:
- The `stay-on-state` macro is used to remain in the `processing` state while updating the progress data to `\"Progress: 50%\"` upon an `update` event.
- Transition to the `completed` state occurs when the `complete` event is triggered, updating the state and its data.

**Notes**

- The `data` parameter is optional but should be structured to fit the requirements of the state model.
- Use this macro to ensure state persistence with updated data when necessary.

Integrate the `stay-on-state` macro into your FSM to handle cases where the state should remain unchanged but its data requires updates. Adjust examples as needed to fit your FSM system.")

(setf (documentation 'when-unhandled 'function)
      "`when-unhandled` Macro

The `when-unhandled` macro defines actions to be executed when an event has not been handled by any prior `stay-on-state` or `goto-state` operations within an FSM (Finite State Machine).

**Parameters**

- `event`: The event that should trigger the body if it remains unhandled by other mechanisms in the FSM.

- `:test`: A key parameter specifying the function used to compare the received event with the specified event. Defaults to `#'eq`, allowing for custom comparison logic.

- `body`: One or more expressions to execute when the specified event is unhandled by `stay-on-state` or `goto-state` actions.

**Description**

The `when-unhandled` macro is designed to catch events that have not been processed by `stay-on-state` or `goto-state`. It provides a fallback mechanism that ensures specific actions are taken for such unhandled events, using a specified test function to determine event equivalency.

**Usage Example**

```lisp
(when-unhandled ('start :test #'eq)
  (log:error \"Start event was unhandled\")
  (notify-admin))

(when-unhandled ('disconnect :test #'eq)
  (log:warn \"Unhandled disconnect event\")
  (attempt-reconnect))
```

In these examples:
- The first block logs an error and notifies an admin if the `start` event remains unhandled, using the default `#'eq` function for testing.
- The second block logs a warning and attempts to reconnect for an unhandled `disconnect` event, also using `#'eq`.

**Notes**

- Utilize the `test` parameter to customize how events are determined as equivalent when necessary.
- `when-unhandled` is essential for capturing and managing scenarios where standard state transitions do not account for all event possibilities.
- Is it possible to use `goto-state` in `body`.

Integrate the `when-unhandled` macro to ensure your FSM handles any unexpected or default cases robustly and flexibly. Adjust the body actions and events as needed for your specific requirements and FSM design.")

(setf (documentation 'on-transition 'function)
      "`on-transition` Macro

The `on-transition` macro defines actions to be executed when a specific state transition occurs within an FSM (Finite State Machine). It uses customizable test functions to determine when a transition has taken place.

**Parameters**

- `transition`: A cons cell or similar paired structure representing the transition, with the car as the starting state and the cdr as the destination state.

- `:test`: A key parameter specifying the function used to compare states. Defaults to `#'eq`, allowing for custom comparison logic if needed.

- `body`: One or more expressions that are executed when the specified transition is detected.

**Description**

The `on-transition` macro provides a mechanism for executing specific actions when the FSM undergoes a particular state transition, as identified by changes in the state model from a starting to an ending state. This macro depends on events being handled by a state transition (`goto-state`), and uses test functions to match state values.

**Usage Example**

```lisp
(on-transition (('idle . 'active) :test #'eq)
  (log:info \"Transitioned from idle to active\")
  (initialize-resources))

(on-transition (('active . 'completed) :test #'eq)
  (log:info \"Transitioned from active to completed\")
  (cleanup-resources))
```

In these examples:
- The first block logs the transition from `idle` to `active` and performs resource initialization when this transition occurs.
- The second block logs the transition from `active` to `completed` and performs cleanup.

**Notes**

- `:test`: Customize the comparison logic to fit the FSM's state representations, especially if using complex or non-standard states.
- The macro relies on transitions being marked by the handling of events through `goto-state`.

Utilize the `on-transition` macro to effectively manage and isolate logic specific to state transitions, ensuring that your FSM operates smoothly and predictably through defined state changes. Adjust the body of transitions to align with the goals and behavior of your FSM system.")
