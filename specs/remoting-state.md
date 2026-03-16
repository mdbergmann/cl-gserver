# Sento Remoting — Implementation State

## Completed

### Phase 1: Project Scaffolding + Serialization + Envelope (done)
- `sento-remoting.asd` — system + test system definitions
- `src/remoting/remoting-api.lisp` — `sento.remoting` package, base `remoting-error` condition
- `src/remoting/serialization.lisp` — `sento.remoting.serialization` package, conditions, protocol generics, `sexp-serializer`
- `src/remoting/envelope.lisp` — `sento.remoting.envelope` package, `envelope` and `error-envelope` structs, `envelope-for-reply`
- `tests/remoting/all-remoting-test.lisp` — test suite root
- `tests/remoting/conditions-test.lisp` — 3 tests (creation, reporting, hierarchy)
- `tests/remoting/serialization-test.lisp` — 6 tests (round-trip atoms, lists, cons, vectors, nil, custom serializer)
- `tests/remoting/envelope-test.lisp` — 5 tests (creation, reply, missing fields, message-types, error-envelope)
- **14 tests, 38 checks, all passing**
- Note: deviated from spec by not having a central `packages.lisp`. Each module defines its own package, following existing project conventions. `remoting-api.lisp` replaces both `packages.lisp` and `conditions.lisp` from the spec.

## Next

### Phase 2: TLS Abstraction + pure-tls Backend
- `src/remoting/tls.lisp` — TLS provider protocol, `tls-config` struct, TLS conditions
- `src/remoting/tls-pure.lisp` — `pure-tls-provider` implementation
- `tests/remoting/tls-test.lisp` — 8 tests
- Needs: `pure-tls` library available
