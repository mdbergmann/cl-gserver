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
- Note: `usocket:connection-refused-error`, `usocket:timeout-error`, `usocket:socket-error` remain package-qualified in `transport-tcp.lisp` to avoid conflicts with identically-named transport conditions. `%transport-message-handler` is internal (not exported), accessed via `rtrans::` from the tcp-transport subclass.

### Phase 4: Remote Actor Reference (done)
- `src/remoting/remote-ref.lisp` — `sento.remoting.remote-ref` package (`:rref`), conditions (`remote-actor-error`, `invalid-remote-uri-error`), `remote-actor-ref` class with internal sender actor for queued `tell`, direct send for `ask-s`/`ask`, correlation-id based response matching, URI parsing (`sento://host:port/path`), `%handle-response` for inbound response routing
- `tests/remoting/remote-ref-test.lisp` — 11 tests (URI parsing valid/invalid, tell enqueues non-blocking, tell message-type correct, tell transport error logged not signaled, ask-s direct send with response, ask-s timeout returns handler-error, ask-s transport error signals immediately, ask returns future resolved via correlation-id, ask timeout resolves future with error, path returns full URI)
- `sento-remoting.asd` — added `remote-ref` module and `remote-ref-test`
- **43 tests, 120 checks, all passing**
- Note: `make-remote-ref` takes explicit `transport` and `serializer` arguments (simplified in Phase 5 via remoting context). `local-sender-path` slot defaults to `"/__local__"`, overridden by Phase 5 to `sento://host:port/__responses__`. Correlation IDs use a thread-safe counter + timestamp (no external UUID library needed). `handle-response`, `pending-asks`, `pending-asks-lock`, and `parse-remote-uri` are public API used by Phase 5 for response routing.

## TODO (post-phase)

- **Max message size:** Currently `tcp-transport` framing has no size limit (4-byte header allows ~4GB). Add a configurable `:max-message-length` to the remoting config (passed through to `tcp-transport`). Check in `%read-frame` (reject before allocating) and `%write-frame` (reject before sending). Sensible default: 16MB.

### Phase 5: Remoting Integration (End-to-End) (done)
- `src/remoting/remoting.lisp` — `sento.remoting` package implementation: `remoting-context` class, `enable-remoting`/`disable-remoting`/`remoting-enabled-p`/`remoting-port` lifecycle API, `make-remote-ref` convenience factory, inbound message routing (`%handle-inbound-tell`, `%handle-inbound-ask`), correlation-id based response routing (`%route-response`). Imports symbols via `eval-when shadowing-import` from `bt2`, `renv`, `rseri`, `rtrans`, `rtrans-tcp`, `rref`, `act`, `ac`, `str` for unqualified use.
- `src/remoting/remote-ref.lisp` — added `local-sender-path` slot to `remote-actor-ref` (defaults to `"/__local__"`, overridden by remoting context to `sento://host:port/__responses__` for response routing). Made `handle-response`, `pending-asks`, `pending-asks-lock`, `parse-remote-uri` public (renamed from `%`-prefixed).
- `tests/remoting/remoting-test.lisp` — 14 integration tests: enable/disable lifecycle (4 tests), make-remote-ref via context (2 tests), tell remote-to-local including complex messages, multiple messages, nested actor paths, non-existent actor (4 tests), ask-s remote-to-local including complex results and timeout (3 tests), ask remote-to-local with future resolution and timeout (2 tests), bidirectional communication between two systems (1 test)
- `sento-remoting.asd` — added `remoting` module and `remoting-test`
- **55 tests, 155 checks, all passing**
- Note: Response routing uses sender-path with format `sento://host:port/__responses__`. The receiving system parses the sender-path URI via `parse-remote-uri` to determine where to send ask-s/ask responses via `transport-send`. The remoting context stores a global mapping (`*system-remoting*`) from actor-system to remoting-context, keyed by identity. Remote-refs created via `rem:make-remote-ref` are registered with the context for correlation-id based response lookup.
