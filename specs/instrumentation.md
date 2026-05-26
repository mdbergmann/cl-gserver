# Sento Instrumentation — Design Specification

## Overview

Instrumentation provides structured observability into the Sento actor system. It answers questions like: How long do actors take to process messages? Which actors are busiest? What is the queue depth? When do actors start and stop?

The design follows two principles:

1. **Zero cost when unused.** No-op generic methods are optimized away by CLOS dispatch. Actors that are not instrumented pay nothing.
2. **Opt-in granularity.** Instrumentation can be enabled per actor type (via CLOS specialization), per actor instance (via mixin), or system-wide (via eventstream publishing).

## Approach

The instrumentation system is layered:

**Layer 1 — Generic function hooks** (foundation). `defgeneric` functions with no-op default methods are called at key points in the actor lifecycle and message flow. Users specialize these on their actor types to collect metrics, log, or publish events.

**Layer 2 — Eventstream integration** (convenience). A built-in `instrumentable` mixin that, when mixed into an actor, automatically publishes instrumentation events to the system eventstream. Users subscribe to event classes they care about.

Layer 1 is always available. Layer 2 is opt-in per actor.

---

## Instrumentation Points

### Actor Lifecycle

| Hook | Called when | Arguments |
|------|-----------|-----------|
| `on-actor-started` | After `pre-start` completes successfully | `(actor)` |
| `on-actor-stopped` | After `after-stop` completes | `(actor)` |

### Message Processing

| Hook | Called when | Arguments |
|------|-----------|-----------|
| `on-message-received` | Message enqueued to message-box | `(actor message message-type)` |
| `on-message-processed` | Message handler returns successfully | `(actor message message-type result duration-ns)` |
| `on-message-failed` | Message handler signals an error | `(actor message message-type condition duration-ns)` |

`message-type` is one of `:tell`, `:ask-s`, `:ask` — derived from `withreply-p` and sender presence.

### Message Box

| Hook | Called when | Arguments |
|------|-----------|-----------|
| `on-queue-count-change` | After enqueue or dequeue | `(actor queue-count)` |

### Dispatcher

| Hook | Called when | Arguments |
|------|-----------|-----------|
| `on-dispatch` | Work dispatched to a worker | `(dispatcher worker-index)` |

---

## Layer 1 — Generic Function Hooks

### Package

```
sento.instrumentation     nickname: :instr
```

### Exports

```lisp
#:on-actor-started
#:on-actor-stopped
#:on-message-received
#:on-message-processed
#:on-message-failed
#:on-queue-count-change
#:on-dispatch
```

### Definitions

```lisp
(defpackage :sento.instrumentation
  (:use :cl)
  (:nicknames :instr)
  (:export #:on-actor-started
           #:on-actor-stopped
           #:on-message-received
           #:on-message-processed
           #:on-message-failed
           #:on-queue-count-change
           #:on-dispatch))

;; ---------------------------------
;; actor lifecycle hooks
;; ---------------------------------

(defgeneric on-actor-started (actor)
  (:documentation "Called after an actor's pre-start completes successfully.")
  (:method ((actor t)) nil))

(defgeneric on-actor-stopped (actor)
  (:documentation "Called after an actor's after-stop completes.")
  (:method ((actor t)) nil))

;; ---------------------------------
;; message processing hooks
;; ---------------------------------

(defgeneric on-message-received (actor message message-type)
  (:documentation "Called when a message is enqueued to the actor's message-box.
MESSAGE-TYPE is one of :tell, :ask-s, :ask.")
  (:method ((actor t) message message-type)
    (declare (ignore message message-type))
    nil))

(defgeneric on-message-processed (actor message message-type result duration-ns)
  (:documentation "Called after a message handler returns successfully.
DURATION-NS is the processing time in nanoseconds.")
  (:method ((actor t) message message-type result duration-ns)
    (declare (ignore message message-type result duration-ns))
    nil))

(defgeneric on-message-failed (actor message message-type condition duration-ns)
  (:documentation "Called when a message handler signals an error.
CONDITION is the signaled condition. DURATION-NS is elapsed time in nanoseconds.")
  (:method ((actor t) message message-type condition duration-ns)
    (declare (ignore message message-type condition duration-ns))
    nil))

;; ---------------------------------
;; message box hooks
;; ---------------------------------

(defgeneric on-queue-count-change (actor queue-count)
  (:documentation "Called after a message is enqueued or dequeued.
QUEUE-COUNT is the current number of messages in the queue.")
  (:method ((actor t) queue-count)
    (declare (ignore queue-count))
    nil))

;; ---------------------------------
;; dispatcher hooks
;; ---------------------------------

(defgeneric on-dispatch (dispatcher worker-index)
  (:documentation "Called when work is dispatched to a worker.
WORKER-INDEX is the index of the selected worker.")
  (:method ((dispatcher t) worker-index)
    (declare (ignore worker-index))
    nil))
```

### Integration Points in Existing Code

**`src/actor.lisp` — `pre-start` method:**

```lisp
;; After existing pre-start logic:
(instr:on-actor-started self)
```

**`src/actor.lisp` — `after-stop` method:**

```lisp
;; After existing after-stop logic:
(instr:on-actor-stopped self)
```

**`src/actor-cell.lisp` — `handle-message`:**

Wrap the message handler call with timing and error handling:

```lisp
(defun %handle-message-with-instrumentation (actor message handler-fun-args withreply-p)
  "Calls the message handler, invoking instrumentation hooks before and after."
  (let ((message-type (%derive-message-type withreply-p)))
    (instr:on-message-received actor message message-type)
    (let ((start (get-internal-real-time)))
      (handler-case
          (let ((result (apply-handler handler-fun-args message)))
            (let ((duration-ns (%elapsed-ns start)))
              (instr:on-message-processed actor message message-type result duration-ns))
            result)
        (error (c)
          (let ((duration-ns (%elapsed-ns start)))
            (instr:on-message-failed actor message message-type c duration-ns))
          (error c))))))

(defun %derive-message-type (withreply-p)
  (if withreply-p :ask-s :tell))

(defun %elapsed-ns (start)
  "Compute elapsed nanoseconds since START (internal-real-time units)."
  (let ((elapsed (- (get-internal-real-time) start)))
    (round (* elapsed (/ 1000000000 internal-time-units-per-second)))))
```

**`src/mbox/message-box.lisp` — `submit` and message processing:**

After enqueue and dequeue operations, call:

```lisp
(instr:on-queue-count-change actor (queue-count queue))
```

Note: The message-box needs a reference to the owning actor. Currently it does not store one. Two options:
1. Add an `actor` slot to `message-box-base` (set during `finalize-initialization`).
2. Pass the actor through handler-fun-args (already partially done — the handler closure captures the actor).

Option 1 is cleaner. Add slot:

```lisp
(defclass message-box-base ()
  ((actor :initform nil
          :accessor actor-of-msgbox
          :documentation "Back-reference to owning actor for instrumentation.")))
```

Set during `finalize-initialization` in `actor-cell.lisp`:

```lisp
(setf (mesgb:actor-of-msgbox (msgbox actor)) actor)
```

**`src/dispatcher.lisp` — `dispatch` / `dispatch-async`:**

After selecting a worker via the router strategy:

```lisp
(instr:on-dispatch dispatcher selected-index)
```

---

## Layer 2 — Eventstream Integration

### Event Classes

Defined in the `sento.instrumentation` package:

```lisp
;; ---------------------------------
;; event base class
;; ---------------------------------

(defclass actor-event ()
  ((actor-path :initarg :actor-path
               :reader actor-path
               :documentation "Path of the actor (e.g. \"/user/my-actor\").")
   (timestamp :initarg :timestamp
              :initform (get-universal-time)
              :reader timestamp
              :documentation "Universal time when the event occurred."))
  (:documentation "Base class for all instrumentation events."))

;; ---------------------------------
;; lifecycle events
;; ---------------------------------

(defclass actor-started-event (actor-event) ()
  (:documentation "Published when an actor starts."))

(defclass actor-stopped-event (actor-event) ()
  (:documentation "Published when an actor stops."))

;; ---------------------------------
;; message events
;; ---------------------------------

(defclass message-event (actor-event)
  ((message-type :initarg :message-type
                 :reader message-type
                 :documentation "One of :tell, :ask-s, :ask."))
  (:documentation "Base class for message-related events."))

(defclass message-received-event (message-event) ()
  (:documentation "Published when a message is enqueued."))

(defclass message-processed-event (message-event)
  ((duration-ns :initarg :duration-ns
                :reader duration-ns
                :documentation "Processing time in nanoseconds."))
  (:documentation "Published when a message is processed successfully."))

(defclass message-failed-event (message-event)
  ((duration-ns :initarg :duration-ns
                :reader duration-ns
                :documentation "Processing time in nanoseconds.")
   (condition-type :initarg :condition-type
                   :reader condition-type
                   :documentation "Type name of the signaled condition."))
  (:documentation "Published when message processing fails."))

;; ---------------------------------
;; queue events
;; ---------------------------------

(defclass queue-count-event (actor-event)
  ((count :initarg :count
          :reader queue-count
          :documentation "Current number of messages in the queue."))
  (:documentation "Published when queue depth changes."))
```

### The `instrumentable` Mixin

```lisp
(defclass instrumentable () ()
  (:documentation "Mixin for actors that publish instrumentation events to the eventstream.
Mix this into an actor class to get automatic event publishing."))
```

CLOS method specializations on `instrumentable` publish events:

```lisp
(defmethod on-actor-started :after ((actor instrumentable))
  (ev:publish (act:context actor)
              (make-instance 'actor-started-event
                             :actor-path (act-cell:name actor))))

(defmethod on-actor-stopped :after ((actor instrumentable))
  (ev:publish (act:context actor)
              (make-instance 'actor-stopped-event
                             :actor-path (act-cell:name actor))))

(defmethod on-message-processed :after ((actor instrumentable) message message-type result duration-ns)
  (declare (ignore message result))
  (ev:publish (act:context actor)
              (make-instance 'message-processed-event
                             :actor-path (act-cell:name actor)
                             :message-type message-type
                             :duration-ns duration-ns)))

(defmethod on-message-failed :after ((actor instrumentable) message message-type condition duration-ns)
  (declare (ignore message))
  (ev:publish (act:context actor)
              (make-instance 'message-failed-event
                             :actor-path (act-cell:name actor)
                             :message-type message-type
                             :condition-type (type-of condition)
                             :duration-ns duration-ns)))
```

---

## User-Facing API

### Direct CLOS specialization (Layer 1)

For users who want full control, specialize the hooks on their actor types:

```lisp
(defclass my-actor (act:actor) ())

(defmethod instr:on-message-processed ((actor my-actor) message message-type result duration-ns)
  (declare (ignore message result))
  (log:info "~a processed ~a in ~,2fms"
            (act-cell:name actor) message-type (/ duration-ns 1e6)))
```

### Eventstream subscription (Layer 2)

For users who want decoupled, system-wide observability:

```lisp
;; Create a metrics-collecting actor
(defvar *metrics* (ac:actor-of *system*
                    :name "metrics-collector"
                    :receive (lambda (event)
                               (typecase event
                                 (instr:message-processed-event
                                  (record-histogram :msg-duration
                                                    (instr:duration-ns event)
                                                    :actor (instr:actor-path event)))
                                 (instr:actor-stopped-event
                                  (record-counter :actor-stops
                                                  :actor (instr:actor-path event)))))))

;; Subscribe to specific event types
(ev:subscribe *system* *metrics* 'instr:message-processed-event)
(ev:subscribe *system* *metrics* 'instr:actor-stopped-event)

;; Create instrumented actors — just add the mixin
(ac:actor-of *system*
  :name "worker"
  :type 'instrumentable   ; or a custom class that includes instrumentable
  :receive (lambda (msg) (process msg)))
```

### Mixing `instrumentable` into custom classes

```lisp
(defclass my-worker (act:actor instr:instrumentable) ()
  (:documentation "A worker actor with automatic instrumentation."))

(ac:actor-of *system*
  :name "worker-1"
  :type 'my-worker
  :receive (lambda (msg) (heavy-computation msg)))
```

---

## Source Files

### Source (`src/instrumentation/`)

```
packages.lisp          — package definition
hooks.lisp             — generic function hooks (Layer 1)
events.lisp            — event classes for eventstream (Layer 2)
instrumentable.lisp    — instrumentable mixin with :after methods
```

### Tests (`tests/instrumentation-test.lisp`)

```
instrumentation-test.lisp  — all instrumentation tests
```

---

## Package Summary

| Package | Nickname | Key Exports |
|---------|----------|-------------|
| `sento.instrumentation` | `:instr` | `on-actor-started`, `on-actor-stopped`, `on-message-received`, `on-message-processed`, `on-message-failed`, `on-queue-count-change`, `on-dispatch`, `instrumentable`, `actor-event`, `actor-started-event`, `actor-stopped-event`, `message-event`, `message-received-event`, `message-processed-event`, `message-failed-event`, `queue-count-event`, `actor-path`, `timestamp`, `message-type`, `duration-ns`, `condition-type`, `queue-count` |

---

## Changes to Existing Code

All changes are additive — no existing behavior is modified.

### `sento.asd`

Add new module to the system definition:

```lisp
(:module "instrumentation"
 :pathname "src/instrumentation"
 :serial t
 :components ((:file "packages")
              (:file "hooks")
              (:file "events")
              (:file "instrumentable")))
```

Place it after `actor-cell` but before `actor` in the dependency order (hooks must be defined before they are called).

### `src/actor-cell.lisp`

- Wrap `handle-message` internals with timing and hook calls.
- This is the single most important integration point — all messages flow through here.

### `src/actor.lisp`

- Add `(instr:on-actor-started self)` at end of `pre-start`.
- Add `(instr:on-actor-stopped self)` at end of `after-stop`.

### `src/mbox/message-box.lisp`

- Add `actor-of-msgbox` slot to `message-box-base`.
- Call `on-queue-count-change` after enqueue/dequeue (requires back-reference to actor).

### `src/actor-cell.lisp` (finalize-initialization)

- Set `actor-of-msgbox` on the message-box after creation.

### `src/dispatcher.lisp`

- Call `on-dispatch` after selecting a worker.

---

## Test Plan

### Layer 1 — Hook tests

- **`on-actor-started--called-after-pre-start`**: Create actor, verify hook is called.
- **`on-actor-stopped--called-after-stop`**: Stop actor, verify hook is called.
- **`on-message-processed--timing`**: Send message, verify hook receives correct message-type and positive duration-ns.
- **`on-message-processed--tell-vs-ask`**: Verify `:tell` and `:ask-s` message-types are correct.
- **`on-message-failed--captures-condition`**: Actor that signals an error, verify hook receives condition.
- **`on-queue-count-change--increments-and-decrements`**: Send messages, verify queue count changes.
- **`on-dispatch--reports-worker-index`**: Use shared dispatcher, verify hook fires.
- **`hooks--no-op-by-default`**: Verify no errors when hooks are not specialized (default no-op).

### Layer 2 — Eventstream integration tests

- **`instrumentable--publishes-lifecycle-events`**: Create instrumentable actor, subscribe, verify events.
- **`instrumentable--publishes-message-events`**: Send messages, verify `message-processed-event` published.
- **`instrumentable--publishes-failure-events`**: Trigger error, verify `message-failed-event` published.
- **`instrumentable--non-instrumented-actors-silent`**: Regular actor produces no events.
- **`eventstream--pattern-matching-on-event-type`**: Subscribe to specific event class, verify filtering works.

### Performance

- **`hooks--no-overhead-when-not-specialized`**: Benchmark message throughput with and without instrumentation. Verify < 1% overhead for non-specialized actors.

---

## Design Decisions

### Why generic functions over callbacks?

CLOS dispatch for a no-op default method is essentially free. Callback functions would require a nil-check on every message. Generic functions also compose naturally with the existing CLOS-based actor hierarchy — users already subclass `actor`, so specializing an instrumentation method is idiomatic.

### Why not always publish to eventstream?

Publishing to the eventstream on every message for every actor would add overhead even when nobody is listening. The mixin approach makes this opt-in. Users who want system-wide instrumentation can create a base class that includes `instrumentable` and use it everywhere.

### Why store actor-path in events, not the actor object?

Events may outlive the actor (e.g., queued in a metrics actor for batching). Storing the path string avoids holding references to stopped actors and is safe to serialize for external metrics systems.

### Why nanoseconds for duration?

Message processing in actor systems can be sub-millisecond. Nanosecond resolution avoids quantization to zero for fast handlers. Users can convert to their preferred unit trivially.

### Why not a separate ASDF system?

Instrumentation is tightly coupled to core internals (`handle-message`, `pre-start`, `after-stop`). Making it a separate system would require exposing internal APIs. Since the hooks are no-ops by default, including them in the core system adds negligible size.
