# Mailbox scheduling for `message-box/dp` — Design Specification

## Overview

Actors on a `:shared` dispatcher use `mesgb:message-box/dp`. Every `submit` pushes
one item onto the message-box queue and then dispatches one unit of work to a
dispatcher worker. That unit pops exactly one item and handles it under the
message-box lock.

Under load this has two costs per message:

1. **One dispatch per message.** Each dispatch is a router selection plus a push
   onto a worker's own queue (lock, enqueue, condition-notify) plus a pop on the
   worker thread. This is paid even when the message-box already has work queued.
2. **Worker contention on one lock.** Eight senders producing for one actor
   schedule eight units of work that land on eight workers, all of which block on
   the same message-box lock. Seven workers are parked on a lock instead of
   serving other actors, and the lock hand-off itself costs a wakeup each time.

Mailbox scheduling replaces "one dispatch per message" with "one dispatch per
burst": a message-box is either *scheduled* or *idle*. A submit on an idle
message-box marks it scheduled and dispatches it once. A submit on a scheduled
message-box only enqueues. The worker that runs the message-box drains up to
`throughput` items in one go, marks the message-box idle, and reschedules it only
when items remain. This is the model Akka uses for its mailboxes.

## Approach

### State

`message-box/dp` gets two new slots:

- `scheduled-p`: an `atomic:atomic-integer`, `0` when idle, `1` when a run is
  scheduled or active.
- `throughput`: the maximum number of items one run handles before yielding the
  worker. Taken from the dispatcher (`disp:throughput`) unless the message-box is
  created with an explicit `:throughput`.

The existing `lock` slot stays. See "Why keep the lock".

### Scheduling

```
submit(item):
  pushq(queue, item)
  schedule()

schedule():
  repeat up to *schedule-max-attempts* times:
    if not CAS(scheduled-p, 0 -> 1): return   ; a run is already scheduled/active
    ok := dispatch-async(dispatcher, exec-fun-args) returns T
    if ok: return
    set scheduled-p 0                          ; nothing will run from this attempt
    if emptyq-p(queue): return                 ; nothing left to strand
    ; else retry: other submitters may have pushed while trusting this run

run():                                   ; dispatcher-exec-fun, on a worker
  unwind-protect
    with lock held:
      repeat throughput times:
        (item, present) := try-popq(queue)
        if not present: break
        if should-run: handle item      ; unchanged handle-popped-item
        else:          finalize item    ; message-box stopped, wake any waiter
  cleanup:
    set scheduled-p 0                    ; CAS 1 -> 0
    if not emptyq-p(queue): schedule()
```

### Lost-wakeup analysis

A producer always pushes before it tries the CAS `0 -> 1`; a run always clears
the flag before it re-checks the queue.

- Producer pushes, run is still active, producer's CAS fails. The run later
  clears the flag and finds the queue non-empty, so it reschedules.
- Producer pushes after the run cleared the flag. The producer's CAS succeeds and
  dispatches. The run's own re-check may or may not see the item; if it does its
  CAS fails, so there is never a second dispatch.
- Both cannot succeed because the flag is a compare-and-swap.

The queue re-check (`emptyq-p`) is done under the queue lock so that the
producer's enqueue, also done under the queue lock, is visible to the run. This
makes the argument depend only on the lock semantics of `bordeaux-threads`, not
on memory-ordering guarantees of the `atomics` library.

### Why keep the lock

With the flag there is at most one active run per message-box, so the lock is not
needed for exclusion in the normal path. It stays for two reasons:

- It gives a portable happens-before edge between the actor state written by one
  run and the next run on a different worker thread.
- A custom `disp:dispatch-async` that runs the function inline, or that returns
  something other than `T` although it did dispatch, can produce two concurrent
  runs. The lock turns that into a slowdown instead of a data race on actor
  state.

Acquiring it once per run instead of once per message removes it from the
per-message cost.

### `dispatch-async` contract

`disp:dispatch-async` returns `T` when the function was handed to a worker.
Any other value (`:stopped` when the chosen worker was stopped, `nil` when the
dispatcher has no workers) means it was not, and the scheduler clears the flag.
While the queue is still non-empty this is retried immediately, up to
`*schedule-max-attempts*` times, since other submitters may have pushed items
while trusting this run to process them; once attempts are exhausted, or the
queue drains, the message-box is left idle for a future submit to try again.
This is documented on the generic function.

### Throughput

`throughput` is configured per dispatcher, like `:workers` and `:strategy`:

```lisp
'(:dispatchers
  (:shared (:workers 4 :throughput 20)))
```

The default is `disp:*default-throughput*`, 5, the same as Akka's default for
this knob. A run handles at most that many items before it yields the worker
and, if the queue is not empty, reschedules the message-box through the router.
This bounds how long one busy actor can hold a worker. The trade-off: a larger
value amortizes the dispatch over more messages; a smaller one gives other
actors on the same dispatcher a turn sooner. Sento's dispatch is more expensive
than a fork-join submit (router selection, a locked push onto the worker queue
and a condition-variable wakeup), so systems with few, busy actors gain from
raising it, see the measurements. A per-message-box override via the
`:throughput` initarg is possible for custom `:mbox-type` constructors.

### Measurements

SBCL 2.6.8 on an Apple M-series machine, 8 sender threads, one actor, 8 workers,
1M `tell` or 200k `ask-s` messages, elapsed until the actor processed all of
them. "before" is the per-message dispatch of version 3.4.6.

| case | before | throughput 1 | 5 | 20 | 100 | pinned |
|---|---|---|---|---|---|---|
| shared `tell` | 366k/s | 282k/s | 659k/s | 851k/s | 912k/s | 1,019k/s |
| shared `ask-s` | 177k/s | 166k/s | 310k/s | 353k/s | 372k/s | 384k/s |

Throughput 1 is slower than before because a message-box now has at most one
active run, where before up to eight workers overlapped their pops on one
message-box. The default of 5 keeps the fairness of the previous behaviour
close while already gaining most of the win; from 20 on the curve flattens.

Not included: a time-based deadline per run (Akka's `throughput-deadline-time`).
Handlers that take long should not run on the shared dispatcher in the first
place, and the item cap already bounds a run for short handlers.

### Fairness

A message-box with a backlog is re-dispatched after each batch and goes through
the router again, so it does not stick to one worker and a backlog on one actor
cannot monopolize more than one worker at a time. The other side of that coin:
with one hot actor the other workers are idle unless other actors have work,
where before the hot actor's lock convoy at least kept them busy waiting. That is
the intended behaviour.

### Stop

`stop` sets `should-run` to `nil` and submits the stop trigger as before. A run
that finds `should-run` cleared finalizes each popped item instead of handling
it, so synchronous callers waiting on an item are woken. Because a run drains a
batch, queued items after a stop are discarded faster than before, where each
discard needed its own dispatch.

## Changes

### `src/queue/queue.lisp`, `src/queue/queue-locked.lisp`

- New generic `try-popq`: returns `(values element t)` when an element was
  available, `(values nil nil)` otherwise. Never blocks. Implemented for
  `queue-bounded` and `queue-unbounded`.
- `emptyq-p` now takes the queue lock. `queued-count` stays lock-free, the
  benchmark reads it on a hot path.

### `src/dispatcher-api.lisp`, `src/dispatcher.lisp`

- `dispatcher-base` gets a `throughput` slot with reader `throughput`.
- `make-dispatcher` reads `:throughput` from the dispatcher config, defaulting to
  `*default-throughput*`.
- `dispatch-async` documents its return value.

### `src/mbox/message-box.lisp`

- `message-box/dp`: slots `scheduled-p`, `throughput`, `exec-fun-args`
  (the `(dispatcher-exec-fun msgbox)` list, built once instead of per submit).
- `submit`: push, then `%schedule`. `dispatch/reply` and `dispatch/noreply`
  collapse into the two branches of `submit`.
- `dispatcher-exec-fun`: batch loop as above.

### `src/actor-system-api.lisp`

- `*default-config*` lists `:throughput` for the `:shared` dispatcher.

## Test plan

- `try-popq--empty` / `try-popq--present` for both queue types.
- `dispatch--batch--one-dispatch-per-burst`: block the handler on the first
  message, send twenty more, release. Count `dispatch-async` invocations with
  cl-mock; expect at most `1 + ceiling(20 / throughput)`, not 21.
- `dispatch--batch--preserves-order`: one sender, one hundred numbered messages,
  processed in order.
- `dispatch--failed-dispatch--does-not-wedge-mailbox`: mock `dispatch-async` to
  return `nil` once; the next `tell` must still be processed.
- `dispatch--failed-dispatch--retries-without-next-submit`: mock `dispatch-async`
  to return `nil` twice then succeed, with only one submit; the item must still
  be processed, since no further submit is coming to retrigger scheduling.
- `dispatch--throughput-from-dispatcher-config`: a system with
  `:throughput 3` gives the message-box that value.
- `dispatch--throughput-limits-run`: with `:workers 1` and `:throughput 2`, a
  second actor's message is handled between batches of the first actor's
  backlog.
- Existing concurrent `ask-s` and timeout tests stay as regression coverage.
