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

### Phase 3: Transport Layer (done)
- `src/remoting/transport.lisp` — `sento.remoting.transport` package (`:rtrans`), transport conditions (`transport-error`, `connection-refused-error`, `connection-timeout-error`, `connection-closed-error`, `send-failed-error`), abstract `transport` base class, protocol generics (`transport-start`, `transport-stop`, `transport-send`)
- `src/remoting/transport-tcp.lisp` — `sento.remoting.transport-tcp` package (`:rtrans-tcp`), `tcp-transport` implementation with 4-byte big-endian length-prefixed framing, TLS support, connection pooling (lazy reconnect), per-connection reader threads, accepted socket tracking for clean shutdown
- `tests/remoting/transport-test.lisp` — 10 tests (start/stop lifecycle, stop closes connections, send envelope loopback, large message >64KB, connection reuse, connection failure reconnect, concurrent sends, connection-refused condition, TLS handshake failure propagation, send over TLS)
- `sento-remoting.asd` — added `bordeaux-threads` dependency; added `transport`, `transport-tcp` modules and `transport-test`
- **32 tests, 89 checks, all passing**
- Note: `usocket:connection-refused-error`, `usocket:timeout-error`, `usocket:socket-error` remain package-qualified in `transport-tcp.lisp` to avoid conflicts with identically-named transport conditions. `transport-message-handler` is exported and imported via `:import-from` in subpackages.

### Phase 4: Remote Actor Reference (done)
- `src/remoting/remote-ref.lisp` — `sento.remoting.remote-ref` package (`:rref`), conditions (`remote-actor-error`, `invalid-remote-uri-error`), `remote-actor-ref` class with sender actor created via `ac:actor-of` on the system's shared dispatcher, direct send for `ask-s`/`ask`, correlation-id based response matching via `%handle-response`, URI parsing (`sento://host:port/path`), `%make-ref-envelope` helper for envelope creation
- `tests/remoting/remote-ref-test.lisp` — 11 tests using FiveAM `def-fixture`/`with-fixture` (URI parsing valid/invalid, tell enqueues non-blocking, tell message-type correct, tell transport error logged not signaled, ask-s direct send with response, ask-s timeout returns handler-error, ask-s transport error signals immediately, ask returns future resolved via correlation-id, ask timeout resolves future with error, path returns full URI)
- `sento-remoting.asd` — added `remote-ref` module and `remote-ref-test`
- **43 tests, 120 checks, all passing**
- Note: `make-remote-ref` takes explicit `transport` and `serializer` arguments. Sender actor uses the system's shared dispatcher (configurable via `:dispatcher` keyword) for scalability. Ask timeouts use the system's wheel-timer via `wt:schedule-once`. Correlation IDs use a thread-safe counter + timestamp. `*local-sender-path*` is a placeholder for ask-s/ask sender-path.

## TODO (post-phase)

- **Max message size:** Currently `tcp-transport` framing has no size limit (4-byte header allows ~4GB). Add a configurable `:max-message-length` to the remoting config (passed through to `tcp-transport`). Check in `%read-frame` (reject before allocating) and `%write-frame` (reject before sending). Sensible default: 16MB.

## Next

### Phase 5: Remoting Integration (End-to-End)
- `src/remoting/remoting.lisp` — actor-system integration, config, lifecycle, inbound message routing
- `tests/remoting/remoting-test.lisp` — full integration tests with two actor-systems communicating
