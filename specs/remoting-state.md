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

### Phase 2: TLS Abstraction + pure-tls Backend (done)
- `src/remoting/tls.lisp` — `sento.remoting.tls` package (`:rtls`), TLS conditions (`tls-error`, `tls-handshake-error`, `tls-certificate-error`, `tls-peer-verify-error`), `tls-config` struct, `tls-wrap`/`tls-unwrap` protocol generics
- `src/remoting/tls-pure.lisp` — `sento.remoting.tls-pure` package, `pure-tls-provider` implementing `tls-wrap`/`tls-unwrap` via `pure-tls` library
- `tests/remoting/tls-test.lisp` — 8 tests (config creation, default peer-verify, unknown provider, dispatch, server/client loopback, mTLS, invalid cert, unwrap)
- `tests/remoting/certs/` — pre-generated test certificates (CA, server, client) for loopback TLS tests
- `tests/remoting/conditions-test.lisp` — updated with 2 additional tests for TLS condition hierarchy and slot/report verification
- `sento-remoting.asd` — added `pure-tls`, `usocket` dependencies; added `tls`, `tls-pure` modules and `tls-test`
- **22 tests, 68 checks, all passing**
- Note: TLS conditions defined in `tls.lisp` alongside the TLS package (consistent with phase 1 pattern). On macOS, `pure-tls:*use-macos-keychain*` is bound to `nil` during wrap operations so that pure Lisp verification is used with custom CA certificates instead of the macOS Security.framework system trust store.

## Next

### Phase 3: Transport Layer
- `src/remoting/transport.lisp` — transport protocol (abstract base class + generics)
- `src/remoting/transport-tcp.lisp` — TCP transport implementation with TLS, framing, connection pooling
- `tests/remoting/transport-test.lisp` — 10 tests
