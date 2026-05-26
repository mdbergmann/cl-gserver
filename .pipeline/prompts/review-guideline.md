# Review guideline — Sento (cl-gserver)

Appended to the built-in REVIEW prompt. Sento is a Common Lisp actor framework
(actors, agents, FSMs, routers, event stream). The built-in prompt covers
general correctness; the rules below are what is *specific to this project* and
must be enforced on the diff under review. The authoritative source is
`CLAUDE.md` at the repo root — read it when in doubt.

## Hard rules — flag any violation

- **Portability is non-negotiable.** Code must be CL-implementation agnostic.
  Flag ANY implementation-specific symbol or package (`sb-ext:`, `sb-thread:`,
  `ccl:`, `ext:`, reader conditionals like `#+sbcl` introducing behavior gaps,
  etc.). Use portable libraries (bordeaux-threads, usocket, …) and standard CL.
- **Naming.** kebab-case for everything. Private/internal functions are
  `%`-prefixed and must NEVER be exported (if it needs exporting, drop the `%`).
  Special/dynamic variables use `*earmuffs*` — never `%` for variables.
- **Packages.** Hierarchical dotted names (`:sento.actor`). `:use :cl` only —
  never `:use` a project package. Prefer `:import-from` and unqualified use over
  package-qualified references; flag a file that mixes `pkg:sym` and unqualified
  access to the same package. Exports use `#:symbol`.
- **CLOS.** `defgeneric` with full `:documentation` lives in the API file;
  `defmethod` in the impl file. Slots include `:initarg`, `:initform`,
  `:reader`/`:accessor`, and `:documentation`. Post-construction setup goes in
  `initialize-instance :after`. Simple value types use `defstruct`.
- **Conditions** inherit from `error`/`serious-condition`, always have a
  `:report`, and slots use `:initarg`/`:reader`.
- **Logging.** log4cl, with context in every message (actor name, message,
  state). Warnings must carry enough to troubleshoot (identifiers, paths, IDs).
- **Actor/concurrency idioms.** Create actors via `ac:actor-of` with a
  dispatcher; flag standalone `message-box/bt` (a dedicated thread per actor).
  Stop `actor-of` actors with `ac:stop`, not `act-cell:stop`. Use the system
  wheel-timer (`wt:schedule-once` on `asys::timeout-timer`) for timeouts — flag
  newly spawned threads used merely to wait.
- **No future/phase/roadmap comments.** Comments describe what the code does now.
- Other smells to flag: missing `(declare (ignore ...))`, `cond`+`typep` where
  `typecase` fits, trivial single-use wrapper functions, trailing whitespace/tabs,
  missing `unwind-protect` cleanup or `handler-case` at boundaries.

## Concurrency correctness (this is an actor framework)

Scrutinize for races, deadlocks, and lifecycle bugs: shared mutable state touched
without synchronization, message-ordering assumptions, blocking inside a receive,
actors/threads/timers that are started but never stopped, and resources not
released on the error path.

## Tests & docs must accompany the change

- New behavior and every bug fix ship with tests in the matching
  `tests/<module>-test.lisp`, registered under `sento.tests:test-suite`. Target
  ~90% coverage. Flag a behavioral change with no test.
- Each test needs a docstring; test names follow `<what>--<variant>` (double
  dashes). Both happy paths and error/failure cases (invalid input, signaled
  conditions, timeouts) should be covered. Async waits use `await-cond` with
  short timeouts; fixtures use `def-fixture`/`with-fixture` and always clean up
  (`ac:shutdown`, `(tell actor :stop)`).
- New **exported** API must be documented in `documentation.lisp` (mgl-pax).
  New user-facing features must add a brief usage example to `README.md`. Flag
  public API or features added without the corresponding docs.

## Not defects here

- Pre-existing issues outside the diff under review.
- The vendored `systems/` tree and `systems.csv` (ocicl local dist) — never
  review or flag dependency source.
