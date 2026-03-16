# Sento (cl-gserver)

Common Lisp actor framework featuring actors, agents, FSMs, routers, and an event stream. Similar in concept to Erlang/Akka.

## Build & Test

```bash
# Load system
(asdf:load-system "sento")

# Run all tests
(asdf:test-system "sento")

# Run a single test suite interactively
(fiveam:run! 'sento.actor-test::actor-tests)
```

Tests use **FiveAM**. The test entry point is `sento.tests:test-suite` defined in `tests/all-test.lisp`. All sub-suites are registered under it via `:in sento.tests:test-suite`.

## Project Structure

```
sento.asd              — system definition (main, tests, docs, bench)
src/                   — source files (loaded serially as specified in .asd)
  atomic/              — atomic operations (platform-specific)
  queue/               — queue implementations (bounded, unbounded)
  mbox/                — message box implementations (bt, dp)
tests/                 — test files (one per source module)
specs/                 — design specifications for new features
systems/               — vendored dependencies
```

## Code Style

### Portability

- Code must be **CL implementation agnostic**. No implementation-specific code (e.g. no `sb-ext:`, `ccl:`, etc.). Use portable libraries and standard CL only.

### Naming

- **kebab-case** everywhere: functions, variables, classes, slots.
- **Private/internal functions:** prefix with `%` (e.g. `%add-actor`, `%merge-config`).
- **Special/dynamic variables:** `*earmuffs*` (e.g. `*self*`, `*state*`, `*sender*`).
- **Classes:** descriptive kebab-case (`actor-cell`, `shared-dispatcher`, `message-box/bt`).

### Packages

- Full hierarchical names with dots: `:sento.actor`, `:sento.actor-context`.
- Short nicknames for convenience: `:act`, `:ac`, `:asys`, `:agt`, `:disp`, `:ev`.
- Explicit `:use :cl` only. No `:use` of project packages — use `:import-from` or qualified names.
- Exports use `#:symbol` notation.
- Use `eval-when (:compile-toplevel)` with `shadowing-import` to resolve symbol conflicts between packages.

### CLOS

- API packages (e.g. `actor-api.lisp`, `actor-context-api.lisp`) define `defgeneric` with full `:documentation`. Implementation files provide the `defmethod`.
- Slot definitions: always include `:initarg`, `:initform`, `:reader` (or `:accessor` for read-write), and `:documentation`.
- Use `initialize-instance :after` for post-construction setup.
- Use `defstruct` for simple value types (e.g. `actor-cell-state`, `fsm-state`).

### Conditions

- Defined with `define-condition`, inheriting from `error` or `serious-condition`.
- Always include a `:report` lambda for readable error messages.
- Slots use `:initarg` and `:reader`.

### Formatting

- 2-space indentation.
- Opening paren on same line as form.
- Slot options aligned vertically.
- Section headers use dashed comment lines:
  ```lisp
  ;; ---------------------------------
  ;; public functions / API
  ;; ---------------------------------
  ```

### Logging

- Uses `log4cl` throughout: `log:trace`, `log:debug`, `log:info`, `log:warn`, `log:error`.
- Always include context in log messages (actor name, message, state).

### Other Conventions

- `handler-case` for error handling at boundaries.
- `unwind-protect` for resource cleanup.
- `alexandria:with-gensyms` in macros for hygiene.
- `(declare (ignore ...))` always explicit — no unused variable warnings.
- No trailing whitespace. No tabs.

## Tests

**Tests are critical. Target 90% coverage for all modules.**

### Structure

- One test file per source module: `src/actor.lisp` → `tests/actor-test.lisp`.
- Test package naming: `:sento.<module>-test` (e.g. `:sento.actor-test`).
- Each test file defines a suite with `def-suite` registered under `sento.tests:test-suite`:
  ```lisp
  (def-suite actor-tests
    :description "actor tests"
    :in sento.tests:test-suite)
  (in-suite actor-tests)
  ```

### Writing Tests

- Every test has a **docstring** describing what it tests.
- Test naming: `<what>--<variant>` with double dashes separating clauses:
  `ask-s--shared--timeout`, `actor-of--from-existing-actor-context`.
- Assertions: `(is ...)` for predicates, `(is-true ...)` / `(is-false ...)` for booleans.

### Fixtures

- Use `def-fixture` for reusable setup/teardown:
  ```lisp
  (def-fixture test-system ()
    (let ((cut (make-actor-system)))
      (unwind-protect
           (&body)
        (ac:shutdown cut))))
  ```
- Always clean up: `ac:shutdown` for systems, `(tell actor :stop)` for standalone actors.
- `&body` marker in fixtures designates where test body is inserted.

### Async Testing

- Use `await-cond` from `miscutils` to poll for async conditions:
  ```lisp
  (is-true (await-cond 0.5 (complete-p future)))
  ```
- First argument is timeout in seconds. Keep timeouts short (0.5–1.0s) for fast tests.

### Mocking

- Use `cl-mock` sparingly — only when real dependencies cannot be used.
- Pattern: `(with-mocks () (answer (fn args) body) ... (invocations 'fn))`.

### Parametrized Tests

- Use `parametrized-test` macro from `sento.test-utils` for testing multiple input combinations:
  ```lisp
  (parametrized-test my-test
      ((param-a param-b)
       (val-1   val-2)
       (val-3   val-4))
    "Docstring."
    (test-body param-a param-b))
  ```
- Generates separate named tests per tuple for clear failure reporting.

### Actor System in Tests

- Create with minimal config: `(make-actor-system '(:dispatchers (:shared (:workers 2))))`.
- Always wrap in `unwind-protect` with `(ac:shutdown system)` in cleanup.
- For standalone actors without a system, use `(make-instance 'mesgb:message-box/bt)` as msgbox.

## Documentation

- **API documentation** (`documentation.lisp`, built via `sento/docs`): All new public API (exported symbols — functions, generics, classes, conditions) must be documented here. Use `mgl-pax` sections and docstrings.
- **README** (`README.md`): New user-facing features must include brief, basic usage examples in the README. Show the simplest working case — enough for a user to get started without reading the full API docs.
