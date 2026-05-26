# Fix guideline — Sento (cl-gserver)

Appended to the built-in FIX prompt. Apply fixes the way this project expects;
the authoritative conventions live in `CLAUDE.md` at the repo root.

## How fixes must be made

- Make the **minimal, targeted** change that resolves the finding. Do not
  refactor unrelated code or restyle untouched lines.
- **Match the surrounding code** — naming (kebab-case, `%`-private,
  `*earmuffs*` for specials), package style (`:import-from`, unqualified use,
  `#:symbol` exports), section-header comments, 2-space indentation, no trailing
  whitespace or tabs.
- **Stay implementation-agnostic.** Never reach for `sb-ext:`/`ccl:`/etc. — use
  portable libraries and standard CL. A fix that only works on one Lisp is wrong.
- Use the established idioms: actors via `ac:actor-of` + a dispatcher (not
  standalone `message-box/bt`), stop via `ac:stop`, timeouts via the wheel-timer
  (`wt:schedule-once`), `handler-case` at boundaries, `unwind-protect` for
  cleanup, `alexandria:with-gensyms` in macros, explicit `(declare (ignore …))`.
- New conditions get a `:report`; new CLOS slots get
  `:initarg`/`:initform`/`:reader`(or `:accessor`)/`:documentation`; new
  generics get `:documentation` in the API file.
- Log with context (actor name, message, state); no future/phase comments.

## Always ship the change complete

- When you fix a bug, **add or extend a regression test** that would have caught
  it, in the matching `tests/<module>-test.lisp` (suite under
  `sento.tests:test-suite`). Give it a docstring and a `<what>--<variant>` name;
  use `def-fixture` + `await-cond` and clean up actors/systems (`ac:shutdown`,
  `(tell actor :stop)`).
- If the fix changes **exported** API, update `documentation.lisp` (mgl-pax) and,
  for user-facing features, add/adjust the `README.md` example in the same change.
- Keep each change small enough to read in a diff. Preserve public APIs unless
  the finding explicitly requires changing them.

## Verifying your work

You may run `sbcl` (allowed in `.pipeline.yml`). Load and test with:

```
sbcl --script .pipeline/run-tests.lisp
```

Dependencies resolve offline from the vendored `systems/` (set up in the image);
do not edit `systems/` or `systems.csv`.
