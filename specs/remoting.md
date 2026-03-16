# Sento Remoting — Design Specification

## Overview

Remoting enables Sento actors to communicate across the network over TLS 1.3.
Actors always live locally in their actor-system. Remoting provides:

1. **Inbound:** The system listens on a TLS endpoint and routes incoming messages to local actors by path.
2. **Outbound:** `remote-actor-ref` is a proxy that implements `tell`/`ask-s`/`ask` but serializes messages and sends them over the network.

The actor's code is identical whether accessed locally or remotely. Only the caller knows the difference — by holding a `remote-actor-ref` instead of an `actor`.

## System Structure

```
sento-remoting.asd            — separate ASDF system (depends on sento, usocket, bordeaux-threads, flexi-streams, pure-tls, log4cl)
sento-remoting/tests           — test system (depends on sento-remoting, fiveam, cl-mock)
```

### Source Files (`src/remoting/`)

```
packages.lisp          — all package definitions
conditions.lisp        — full condition hierarchy
serialization.lisp     — serialization protocol + default s-expression impl
envelope.lisp          — network message envelope (struct)
tls.lisp               — TLS provider protocol (generics)
tls-pure.lisp          — pure-tls backend implementation
transport.lisp         — transport protocol (abstract base class + generics)
transport-tcp.lisp     — TCP transport implementation with TLS, framing, connection pooling
remote-ref.lisp        — remote actor proxy implementing tell/ask-s/ask
remoting.lisp          — actor-system integration, config, lifecycle
text-protocol.lisp     — interactive text protocol handler, server, and REPL client
```

### Test Files (`tests/remoting/`)

```
all-remoting-test.lisp      — suite definition
conditions-test.lisp        — condition hierarchy, creation, reporting
serialization-test.lisp     — round-trip for all supported types, edge cases
envelope-test.lisp          — envelope creation, field access, serialization round-trip
tls-test.lisp               — TLS wrap/unwrap, certificate validation, error cases
transport-test.lisp         — connection lifecycle, send/receive, reconnection, framing
remote-ref-test.lisp        — tell/ask-s/ask via remote ref, timeout handling
remoting-test.lisp          — full integration: two actor-systems talking over network
text-protocol-test.lisp     — text commands, REPL client, error responses
```

---

## User-Facing API

### Making a local actor reachable remotely

The actor is created normally. Remoting makes it addressable from the outside — it is the system that listens, not the actor.

```lisp
;; Node A (192.168.1.1:4711)
(defvar *system* (asys:make-actor-system))

;; Enable remoting — this starts the TLS listener
(rem:enable-remoting *system*
  :host "0.0.0.0"
  :port 4711
  :tls (:certificate "cert.pem"
        :private-key "key.pem"
        :ca-certificate "ca.pem"))

;; Create a normal actor — nothing special
(ac:actor-of *system*
  :name "greeter"
  :receive (lambda (msg)
             (format nil "Hello, ~a!" msg)))
```

The actor `/user/greeter` is now reachable from any remote system that connects to `192.168.1.1:4711`.

### Talking to a remote actor from another node

```lisp
;; Node B (192.168.1.2:4712)
(defvar *system* (asys:make-actor-system))

(rem:enable-remoting *system*
  :host "0.0.0.0"
  :port 4712
  :tls (:certificate "cert.pem"
        :private-key "key.pem"
        :ca-certificate "ca.pem"))

;; Get a reference to the remote actor on Node A
(defvar *greeter* (rem:make-remote-ref *system*
                    "sento://192.168.1.1:4711/user/greeter"))

;; Use it exactly like a local actor
(act:tell *greeter* "World")                        ; fire-and-forget
(act:ask-s *greeter* "World" :time-out 5)           ; => "Hello, World!"
(act:ask *greeter* "World")                         ; => future
```

### Bidirectional communication

When a remote actor needs to reply back or call actors on the caller's system, the sender-path in the envelope carries the origin's `sento://host:port/path`. The inbound handler constructs a `remote-actor-ref` from the envelope's `sender-path` and binds it to `*sender*` before dispatching.

```lisp
;; Node A — actor that calls back to sender
(ac:actor-of *system*
  :name "processor"
  :receive (lambda (msg)
             ;; *sender* here is a remote-actor-ref pointing back to Node B
             (let ((result (process msg)))
               (act:tell *sender* result))))
```

---

## Design Principles

1. **Location transparency** — `tell`/`ask-s`/`ask` work the same on local actors and remote refs.
2. **Actors don't know they're remote** — remoting is a system-level concern, not an actor concern.
3. **TLS by default** — all connections use TLS 1.3 with mutual authentication (mTLS).
4. **Pluggable layers** — serializer and TLS provider are swappable via generic protocols.
5. **Minimal API surface** — `enable-remoting`, `disable-remoting`, `make-remote-ref`.

---

## Package Summary

| Package | Nickname | Key Exports |
|---------|----------|-------------|
| `sento.remoting.serialization` | `:rseri` | `serialize`, `deserialize`, `sexp-serializer`, `serialization-error`, `deserialization-error` |
| `sento.remoting.envelope` | `:renv` | `envelope`, `make-envelope`, `envelope-for-reply`, `error-envelope`, accessors |
| `sento.remoting.tls` | `:rtls` | `tls-wrap`, `tls-unwrap`, `tls-config`, `make-tls-config`, `tls-error`, `tls-handshake-error`, `tls-certificate-error`, `tls-peer-verify-error` |
| `sento.remoting.tls-pure` | — | `pure-tls-provider` |
| `sento.remoting.transport` | `:rtrans` | `transport`, `tcp-transport`, `transport-start`, `transport-stop`, `transport-send`, `transport-error`, `connection-refused-error`, `connection-timeout-error`, `connection-closed-error`, `send-failed-error` |
| `sento.remoting.remote-ref` | `:rref` | `remote-actor-ref`, `make-remote-ref`, `remote-actor-error`, `remote-actor-not-found-error`, `invalid-remote-uri-error` |
| `sento.remoting` | `:rem` | `enable-remoting`, `disable-remoting`, `make-remote-ref`, `remoting-error` |
| `sento.remoting.text-protocol` | `:rtp` | `start-text-server`, `stop-text-server`, `connect-repl`, `handle-text-connection` |
| `sento.remoting.tests` | — | `remoting-test-suite` |

## Dependency Graph

```
sento-remoting
+-- sento              (actor framework)
+-- usocket            (TCP sockets)
+-- bordeaux-threads   (threading)
+-- flexi-streams      (binary/text streams)
+-- pure-tls           (TLS 1.3)
+-- log4cl             (logging)
```

---
---

# Implementation Phases

Each phase is independently buildable, testable, and produces a working increment. Phases must be implemented in order — each builds on the previous.

---

## Phase 1: Project Scaffolding + Serialization + Envelope

**Goal:** Establish the ASDF system, package structure, and the two foundational data layers that have no external dependencies beyond sento itself.

### What to Build

#### 1.1 ASDF System Definition (`sento-remoting.asd`)

Define `sento-remoting` and `sento-remoting/tests` systems. Initially only include the modules built in this phase. Modules for later phases are added as they are implemented.

#### 1.2 Package Definitions (`src/remoting/packages.lisp`)

Define all packages up front (even those implemented later) so that forward references compile. Packages implemented in later phases export their symbols but have no implementation yet.

#### 1.3 Condition Hierarchy (`src/remoting/conditions.lisp`)

Define the full condition hierarchy. All conditions are defined here in one place so that any layer can signal them. This avoids circular dependencies between layers.

```lisp
(define-condition remoting-error (error)
  ((message :initarg :message :reader remoting-error-message))
  (:report (lambda (c stream)
             (format stream "Remoting error: ~a" (remoting-error-message c)))))

;; --- Serialization conditions ---

(define-condition serialization-error (remoting-error)
  ((object :initarg :object :reader serialization-error-object))
  (:documentation "Signaled when an object cannot be serialized."))

(define-condition deserialization-error (remoting-error)
  ((bytes :initarg :bytes :reader deserialization-error-bytes))
  (:documentation "Signaled when received bytes cannot be deserialized — corrupt data, incompatible serializer."))

;; --- TLS conditions ---

(define-condition tls-error (remoting-error) ())

(define-condition tls-handshake-error (tls-error)
  ((reason :initarg :reason :reader tls-handshake-error-reason))
  (:report (lambda (c stream)
             (format stream "TLS handshake failed: ~a" (tls-handshake-error-reason c))))
  (:documentation "Signaled when TLS handshake fails — protocol mismatch, cipher negotiation failure, etc."))

(define-condition tls-certificate-error (tls-error)
  ((reason :initarg :reason :reader tls-certificate-error-reason)
   (certificate-subject :initarg :certificate-subject
                        :reader tls-certificate-error-subject
                        :initform nil))
  (:report (lambda (c stream)
             (format stream "TLS certificate error: ~a (subject: ~a)"
                     (tls-certificate-error-reason c)
                     (tls-certificate-error-subject c))))
  (:documentation "Signaled on certificate validation failure: expired, untrusted CA, hostname mismatch, revoked, self-signed when not allowed."))

(define-condition tls-peer-verify-error (tls-error)
  ()
  (:documentation "Signaled when mutual TLS is required but peer did not present a client certificate."))

;; --- Transport conditions ---

(define-condition transport-error (remoting-error) ())

(define-condition connection-refused-error (transport-error)
  ((host :initarg :host :reader connection-refused-host)
   (port :initarg :port :reader connection-refused-port))
  (:report (lambda (c stream)
             (format stream "Connection refused to ~a:~a"
                     (connection-refused-host c) (connection-refused-port c))))
  (:documentation "Signaled when TCP connection to remote host is refused."))

(define-condition connection-timeout-error (transport-error)
  ((host :initarg :host :reader connection-timeout-host)
   (port :initarg :port :reader connection-timeout-port))
  (:documentation "Signaled when TCP connection attempt times out."))

(define-condition connection-closed-error (transport-error)
  ()
  (:documentation "Signaled when an established connection is unexpectedly closed by the remote side."))

(define-condition send-failed-error (transport-error)
  ((envelope :initarg :envelope :reader send-failed-envelope))
  (:documentation "Signaled when sending an envelope fails after connection was established."))

;; --- Remote actor conditions ---

(define-condition remote-actor-error (remoting-error) ())

(define-condition remote-actor-not-found-error (remote-actor-error)
  ((path :initarg :path :reader remote-actor-not-found-path))
  (:report (lambda (c stream)
             (format stream "Remote actor not found: ~a" (remote-actor-not-found-path c))))
  (:documentation "Signaled on the caller side when the remote system cannot find the target actor."))

(define-condition invalid-remote-uri-error (remote-actor-error)
  ((uri :initarg :uri :reader invalid-remote-uri))
  (:documentation "Signaled when parsing a sento:// URI fails."))
```

#### 1.4 Serialization (`src/remoting/serialization.lisp`)

**Package:** `sento.remoting.serialization` (nickname `:rseri`)

Protocol:

```lisp
(defgeneric serialize (serializer object)    ; => byte-vector
(defgeneric deserialize (serializer bytes)   ; => object
```

The serializer is pluggable via CLOS dispatch. Users implement these two methods on their own serializer class to use any wire format (Protocol Buffers, MessagePack, CBOR, etc.).

Default implementation `sexp-serializer`: writes objects as s-expressions via `write-to-string` / `read-from-string` with `flexi-streams` for UTF-8 encoding. Supported types: symbols, strings, numbers, lists, cons cells, keywords, characters, vectors, hash-tables (as alists).

Only the `message` payload inside the envelope is handled by the pluggable serializer. The envelope metadata (target-path, sender-path, message-type, correlation-id) is always serialized as s-expressions with length-prefixed framing. This means:

- Both communicating systems must use the same message serializer.
- The envelope framing is fixed and not user-configurable.

`enable-remoting` accepts a `:serializer` keyword argument:

- **Omitted** — defaults to `(make-instance 'rseri:sexp-serializer)`.
- **Instance** — any object implementing `rseri:serialize` and `rseri:deserialize`.

```lisp
;; Default — uses sexp-serializer
(rem:enable-remoting *system* :host "0.0.0.0" :port 4711 :tls (...))

;; Explicit default
(rem:enable-remoting *system*
  :serializer (make-instance 'rseri:sexp-serializer) ...)

;; Custom
(rem:enable-remoting *system*
  :serializer (make-instance 'my-protobuf-serializer) ...)
```

Custom serializer example (Protocol Buffers):

```lisp
;; User defines their own serializer class
(defclass protobuf-serializer () ())

(defmethod rseri:serialize ((s protobuf-serializer) object)
  (my-proto:encode object))

(defmethod rseri:deserialize ((s protobuf-serializer) bytes)
  (my-proto:decode bytes))
```

#### 1.5 Envelope (`src/remoting/envelope.lisp`)

**Package:** `sento.remoting.envelope` (nickname `:renv`)

```lisp
(defstruct envelope
  target-path       ; "/user/my-actor" — path on remote system
  sender-path       ; "sento://host:port/user/caller" — for reply routing
  message           ; the serialized payload (byte vector)
  message-type      ; :tell, :ask-s, :ask
  correlation-id)   ; UUID string — for matching ask responses to pending futures
```

Functions:

- `make-envelope` — convenience constructor
- `envelope-for-reply` — creates a response envelope swapping target/sender and carrying the correlation-id

Error envelope for remote-side failures:

```lisp
(defstruct error-envelope
  correlation-id       ; matches the original request
  error-type           ; keyword: :actor-not-found, :handler-error, :deserialization-error
  error-message)       ; human-readable description
```

Envelopes themselves are serialized as s-expressions for framing. The `message` field within is already a byte vector from the serializer.

### Files Created

| File | Type |
|------|------|
| `sento-remoting.asd` | system definition |
| `src/remoting/packages.lisp` | all package definitions |
| `src/remoting/conditions.lisp` | full condition hierarchy |
| `src/remoting/serialization.lisp` | protocol + sexp-serializer |
| `src/remoting/envelope.lisp` | envelope + error-envelope structs |
| `tests/remoting/all-remoting-test.lisp` | test suite root |
| `tests/remoting/conditions-test.lisp` | condition creation and reporting |
| `tests/remoting/serialization-test.lisp` | serialization tests |
| `tests/remoting/envelope-test.lisp` | envelope tests |

### Tests (Phase 1): 14 tests

**conditions-test (~3):**
- Create each condition type, verify slots and `:report` output
- Condition hierarchy: `(typep (make-condition 'tls-handshake-error ...) 'remoting-error)` is true
- All conditions are subtypes of `remoting-error`

**serialization-test (~6):**
- Round-trip atoms (numbers, strings, symbols, keywords, characters)
- Round-trip nested lists
- Round-trip cons cells
- Round-trip vectors
- Round-trip nil / empty list
- Custom serializer class dispatches correctly

**envelope-test (~5):**
- Create envelope, verify all fields
- `envelope-for-reply` swaps paths and preserves correlation-id
- Serialize/deserialize envelope round-trip
- Missing required fields signal condition
- Message-type restricted to `:tell` / `:ask-s` / `:ask`

### Done When

- `(asdf:load-system "sento-remoting")` compiles without errors
- `(asdf:test-system "sento-remoting")` runs 14 tests, all pass

---

## Phase 2: TLS Abstraction + pure-tls Backend

**Goal:** Pluggable TLS provider protocol and the pure-tls implementation. After this phase, you can establish encrypted connections on loopback.

**Prerequisite:** Phase 1 complete. `pure-tls` library available in Quicklisp/local projects.

### What to Build

#### 2.1 TLS Provider Protocol (`src/remoting/tls.lisp`)

**Package:** `sento.remoting.tls` (nickname `:rtls`)

```lisp
(defgeneric tls-wrap (provider socket &key certificate private-key
                                         ca-certificate hostname
                                         peer-verify role)
  ;; role is :server or :client
  ;; => wrapped-stream

(defgeneric tls-unwrap (provider wrapped-stream)
  ;; => nil, cleanup
```

Configuration:

```lisp
(defstruct tls-config
  provider           ; :pure-tls (or :cl+ssl in future)
  certificate        ; path to cert PEM
  private-key        ; path to key PEM
  ca-certificate     ; path to CA PEM for peer verification
  (peer-verify t))   ; enable mutual TLS by default
```

#### 2.2 pure-tls Backend (`src/remoting/tls-pure.lisp`)

**Package:** `sento.remoting.tls-pure`

```lisp
(defclass pure-tls-provider () ())

(defmethod tls-wrap ((provider pure-tls-provider) socket
                     &key certificate private-key ca-certificate
                          hostname peer-verify role)
  ;; Use pure-tls API to wrap the usocket stream
  ;; For :server role — TLS accept with server cert
  ;; For :client role — TLS connect with optional client cert (mTLS)
  ;; Signals tls-handshake-error, tls-certificate-error, tls-peer-verify-error on failure
  ...)

(defmethod tls-unwrap ((provider pure-tls-provider) stream)
  ;; Close TLS session gracefully
  ...)
```

### Files Created/Modified

| File | Action |
|------|--------|
| `src/remoting/tls.lisp` | create |
| `src/remoting/tls-pure.lisp` | create |
| `tests/remoting/tls-test.lisp` | create |
| `sento-remoting.asd` | add tls, tls-pure modules |
| `src/remoting/packages.lisp` | add tls package exports |

### Tests (Phase 2): 8 tests

**tls-test — protocol tests (~4):**
- `tls-config` creation with all fields
- Default peer-verify is `t`
- Calling `tls-wrap` with unknown provider signals condition
- Protocol generic function dispatch works (mock provider)

**tls-test — pure-tls backend tests (~4):**
- Server-side TLS wrap with self-signed cert (loopback)
- Client-side TLS connect to loopback server
- mTLS: client presents cert, server verifies
- Invalid cert — `tls-certificate-error` signaled

### Test Infrastructure

Tests need self-signed certificates. Create a test fixture that generates temporary certs using openssl CLI (or provide pre-generated test certs in `tests/remoting/certs/`).

### Done When

- A TLS-encrypted stream can be established between two usocket endpoints on localhost
- All 8 tests pass
- Cumulative: 22 tests passing

---

## Phase 3: Transport Layer

**Goal:** TCP listener and sender with TLS, length-prefixed framing, and connection pooling. After this phase, two transport instances can exchange envelopes over encrypted connections.

**Prerequisite:** Phase 2 complete.

### What to Build

#### 3.1 Transport Protocol (`src/remoting/transport.lisp`)

**Package:** `sento.remoting.transport` (nickname `:rtrans`)

The transport is a pluggable abstraction. The generic protocol defines what any transport must implement — TCP is the default, but WebSocket or other transports can be added later by subclassing.

**Abstract base class:**

```lisp
(defclass transport ()
  ((host port tls-config tls-provider
    message-handler      ; function called with (envelope) for inbound messages
    running-p))
  (:documentation "Abstract base class for remoting transports.
Subclasses implement the actual connection and framing logic."))
```

**Protocol (generic functions):**

```lisp
(defgeneric transport-start (transport message-handler-fn)
  (:documentation "Start listening for inbound connections. message-handler-fn is called with each received envelope."))

(defgeneric transport-stop (transport)
  (:documentation "Stop listener and close all connections."))

(defgeneric transport-send (transport target-host target-port envelope)
  (:documentation "Send an envelope to a remote host. Manages connections internally."))
```

#### 3.2 TCP Transport (`src/remoting/transport-tcp.lisp`)

**Default implementation:** `tcp-transport` subclass of `transport`.

```lisp
(defclass tcp-transport (transport)
  ((listener-socket listener-thread
    connections          ; hash-table: "host:port" -> connection
    ))
  (:documentation "TCP transport with TLS and length-prefixed framing."))
```

**Framing:** 4-byte big-endian length prefix + payload bytes.

**Connection pooling:** Outbound connections are cached in `connections` hash-table. Reconnect on failure (lazy, on next send attempt).

**Listener loop:** Accept connections in a thread, spawn a reader thread per connection. Each reader thread: read frame -> deserialize envelope -> call `message-handler`.

**Error signaling:**
- `connection-refused-error` when TCP connect fails
- `connection-timeout-error` when TCP connect times out
- `connection-closed-error` when remote side drops
- `send-failed-error` when write to socket fails

**Adding a custom transport (e.g. WebSocket in the future):**

```lisp
(defclass websocket-transport (transport)
  ((...)))

(defmethod transport-start ((transport websocket-transport) handler-fn) ...)
(defmethod transport-stop ((transport websocket-transport)) ...)
(defmethod transport-send ((transport websocket-transport) host port envelope) ...)

;; Use it:
(rem:enable-remoting *system*
  :transport (make-instance 'my-websocket-transport ...) ...)
```

### Files Created/Modified

| File | Action |
|------|--------|
| `src/remoting/transport.lisp` | create (abstract protocol) |
| `src/remoting/transport-tcp.lisp` | create (TCP implementation) |
| `tests/remoting/transport-test.lisp` | create |
| `sento-remoting.asd` | add transport, transport-tcp modules |

### Tests (Phase 3): 10 tests

**transport-test (~10):**
- Start/stop lifecycle (listener binds and unbinds)
- Send envelope between two transports on loopback
- Framing: write/read length-prefixed messages correctly
- Large message (> 64KB)
- Connection reuse (second send reuses cached connection)
- Connection failure -> reconnect on next send
- Concurrent sends from multiple threads
- Stop transport -> pending connections closed
- Connection to non-listening port -> `connection-refused-error`
- TLS handshake failure -> `tls-handshake-error` propagated

### Done When

- Two transport instances on localhost can exchange envelopes over TLS
- Connection pooling works (verified by test)
- All 10 tests pass
- Cumulative: 32 tests passing

---

## Phase 4: Remote Actor Reference

**Goal:** A proxy that implements the Sento actor protocol (`tell`/`ask-s`/`ask`) but sends messages over the transport layer. After this phase, you can send messages to a remote actor path — but there is no inbound routing yet on the receiving side.

**Prerequisite:** Phase 3 complete.

### What to Build

#### 4.1 Remote Actor Reference (`src/remoting/remote-ref.lisp`)

**Package:** `sento.remoting.remote-ref` (nickname `:rref`)

#### Design: Local Send Queue for `tell`

Following the pattern established by Erlang/OTP and Akka, `tell` on a remote ref must be non-blocking — consistent with local actor `tell` behavior (location transparency). To achieve this, the remote-actor-ref uses an **internal sender actor** whose message box queues outbound `tell` messages. The sender actor's receive function performs the actual network I/O.

`ask-s` and `ask` bypass the send queue and send directly on the caller's thread. Rationale:
- `ask-s` blocks the caller anyway — queueing would add latency (waiting behind queued tells) with no benefit.
- `ask` returns a future — the caller gets immediate transport error feedback, and the correlation-id mechanism handles response routing without needing an intermediate actor (unlike local `ask` which uses an anonymous `async-waitor-actor`).

```
remote-actor-ref
  |
  +-- sender-actor (pinned, owns a thread)
  |     mbox with queue (bounded or unbounded)
  |     receive = serialize + transport-send
  |
  +-- pending-asks (hash-table: correlation-id -> condvar/future)
  +-- transport (shared, from remoting context)
  +-- serializer
```

```lisp
(defclass remote-actor-ref ()
  ((remote-host       :initarg :remote-host
                      :reader remote-host)
   (remote-port       :initarg :remote-port
                      :reader remote-port)
   (target-path       :initarg :target-path
                      :reader target-path)
   (transport         :initarg :transport
                      :reader transport)
   (serializer        :initarg :serializer
                      :reader serializer)
   (sender-actor      :reader sender-actor
                      :documentation "Internal pinned actor that queues and sends tell messages.")
   (pending-asks      :initform (make-hash-table :test 'equal)
                      :reader pending-asks
                      :documentation "Hash-table: correlation-id -> condvar (ask-s) or future (ask).")
   (system            :initarg :system
                      :reader system))
  (:documentation "Proxy for a remote actor. Implements tell/ask-s/ask but sends over the network."))
```

#### `tell` — queued via internal sender actor (non-blocking)

```lisp
(defmethod act:tell ((ref remote-actor-ref) message &optional sender)
  ;; Build envelope, hand it to the internal sender actor.
  ;; Returns immediately — sender actor processes queue in background.
  (let ((envelope (make-envelope :target-path (target-path ref)
                                 :sender-path (derive-sender-path sender ref)
                                 :message (serialize (serializer ref) message)
                                 :message-type :tell)))
    (act:tell (sender-actor ref) envelope)))
```

The sender actor's receive function:

```lisp
;; Created during initialize-instance :after on remote-actor-ref
(lambda (envelope)
  (handler-case
      (transport-send (transport ref) (remote-host ref) (remote-port ref) envelope)
    (transport-error (c)
      (log:warn "Remote tell send failed for ~a: ~a"
                (envelope-target-path envelope) c)
      ;; Future: publish to event stream as dead letter
      )))
```

#### `ask-s` — direct send, blocks on condvar (bypasses queue)

```lisp
(defmethod act:ask-s ((ref remote-actor-ref) message &key time-out)
  ;; Send directly on caller's thread — transport errors signal immediately.
  ;; Block on condvar until response envelope arrives (matched by correlation-id)
  ;; or timeout expires.
  (let* ((corr-id (make-uuid))
         (envelope (make-envelope :target-path (target-path ref)
                                  :sender-path (derive-sender-path nil ref)
                                  :message (serialize (serializer ref) message)
                                  :message-type :ask-s
                                  :correlation-id corr-id))
         (lock (bt2:make-lock))
         (cvar (bt2:make-condition-variable)))
    ;; Register pending ask before sending
    (setf (gethash corr-id (pending-asks ref))
          (list :ask-s lock cvar :no-result))
    ;; Send on caller's thread — transport errors propagate directly
    (transport-send (transport ref) (remote-host ref) (remote-port ref) envelope)
    ;; Block until response or timeout
    (bt2:with-lock-held (lock)
      (bt2:condition-wait cvar lock :timeout time-out))
    ;; Retrieve and clean up
    (let ((entry (gethash corr-id (pending-asks ref))))
      (remhash corr-id (pending-asks ref))
      (let ((result (fourth entry)))
        (if (eq result :no-result)
            (cons :handler-error 'ask-timeout)
            result)))))
```

#### `ask` — direct send, returns future (bypasses queue)

No anonymous `async-waitor-actor` needed (unlike local `ask`). The correlation-id in the envelope serves as the return address — the inbound handler matches it in `pending-asks` and resolves the future directly.

```lisp
(defmethod act:ask ((ref remote-actor-ref) message &key time-out)
  ;; Send directly, return future.
  ;; Correlation-id replaces the anonymous waitor actor pattern used locally.
  (let* ((corr-id (make-uuid))
         (envelope (make-envelope :target-path (target-path ref)
                                  :sender-path (derive-sender-path nil ref)
                                  :message (serialize (serializer ref) message)
                                  :message-type :ask
                                  :correlation-id corr-id))
         (future (make-future)))
    ;; Register future for correlation-id
    (setf (gethash corr-id (pending-asks ref))
          (list :ask future))
    ;; Send on caller's thread — transport errors propagate directly
    (transport-send (transport ref) (remote-host ref) (remote-port ref) envelope)
    ;; Schedule timeout cleanup if time-out specified
    (when time-out
      (%schedule-ask-timeout ref corr-id future time-out))
    future))
```

#### Response handling (inbound)

When a response envelope arrives (via the transport's inbound handler), it is matched to a pending ask:

```lisp
(defun %handle-response (ref envelope)
  "Called by inbound handler when a response envelope arrives."
  (let* ((corr-id (envelope-correlation-id envelope))
         (entry (gethash corr-id (pending-asks ref))))
    (when entry
      (case (first entry)
        (:ask-s
         ;; Signal the waiting condvar
         (let ((lock (second entry))
               (cvar (third entry))
               (result (deserialize (serializer ref) (envelope-message envelope))))
           (setf (fourth entry) result)
           (bt2:with-lock-held (lock)
             (bt2:condition-notify cvar))))
        (:ask
         ;; Resolve the future
         (let ((future (second entry))
               (result (deserialize (serializer ref) (envelope-message envelope))))
           (remhash corr-id (pending-asks ref))
           (fulfill future result)))))))
```

#### Backpressure

The internal sender actor can use a bounded message box. When the queue is full, `tell` on the remote ref raises `queue-full-error` — the same backpressure mechanism local actors already have. Configurable via `make-remote-ref`:

```lisp
(rem:make-remote-ref system "sento://host:port/path"
  :max-queue-size 10000)  ; nil = unbounded (default)
```

#### Factory

```lisp
(defun make-remote-ref (system uri &key max-queue-size)
  ;; Parse "sento://host:port/user/actor-path"
  ;; Create internal sender actor with pinned mbox
  ;; Return remote-actor-ref connected via system's transport
  ...)
```

URI parsing: extract host, port, and actor path from `sento://host:port/path`. Signal `invalid-remote-uri-error` on malformed input.

### Files Created/Modified

| File | Action |
|------|--------|
| `src/remoting/remote-ref.lisp` | create |
| `tests/remoting/remote-ref-test.lisp` | create |
| `sento-remoting.asd` | add remote-ref module |

### Tests (Phase 4): 11 tests

**remote-ref-test (~11):**
- Parse `sento://` URI correctly (host, port, path extracted)
- Invalid URI signals `invalid-remote-uri-error`
- `tell` enqueues to sender actor and returns immediately (non-blocking)
- `tell` envelope arrives at transport with correct `:tell` message-type
- `tell` transport error is logged, not signaled to caller
- `ask-s` sends directly (bypasses sender actor queue) and returns response
- `ask-s` timeout returns `(cons :handler-error ask-timeout)`
- `ask-s` transport error signals immediately to caller
- `ask` returns a future, response resolves it via correlation-id
- `ask` timeout resolves future with error
- `act:path` on remote-ref returns full `sento://` URI

### Done When

- `make-remote-ref` parses URIs and creates ref instances with internal sender actor
- `tell` enqueues to sender actor (non-blocking, consistent with local `tell`)
- `ask-s`/`ask` send directly and handle responses via correlation-id
- All 11 tests pass
- Cumulative: 43 tests passing

---

## Phase 5: Remoting Integration (End-to-End)

**Goal:** Wire everything together. `enable-remoting` starts a listener that routes inbound messages to local actors. Two actor-systems can communicate over the network. This is the final phase.

**Prerequisite:** Phase 4 complete.

### What to Build

#### 5.1 Remoting Integration (`src/remoting/remoting.lisp`)

**Package:** `sento.remoting` (nickname `:rem`)

Configuration:

```lisp
;; Serializer is an instance, not a keyword.
;; When omitted, defaults to (make-instance 'rseri:sexp-serializer).
(:remoting (:enabled t
            :host "0.0.0.0"
            :port 4711
            :tls (:certificate "/path/cert.pem"
                  :private-key "/path/key.pem"
                  :ca-certificate "/path/ca.pem"
                  :peer-verify t)))
```

Public API:

```lisp
(defun enable-remoting (actor-system &key host port tls-config
                                          (serializer (make-instance 'rseri:sexp-serializer))
                                          (transport nil)
                                          (text-port nil))
  ;; When transport is nil, creates a tcp-transport by default
  ;; When transport is provided, uses the given transport instance
  ;; Creates transport, starts listener
  ;; Registers inbound message-handler that:
  ;;   1. Deserializes envelope
  ;;   2. Resolves target-path via ac:find-actors
  ;;   3. Dispatches via tell/ask-s/ask to local actor
  ;;   4. For :ask/:ask-s — sends response envelope back
  ;;   5. For inbound with sender-path — constructs remote-actor-ref and binds to *sender*
  ;; Returns the remoting context (for shutdown)
  ...)

(defun disable-remoting (remoting-context)
  ;; Stop transport, cleanup connections
  ...)

(defun make-remote-ref (system uri)
  ;; Public API: create a remote actor reference
  ...)
```

Inbound message handler:

```lisp
(defun handle-inbound (system transport envelope)
  (let ((actors (ac:find-actors system (envelope-target-path envelope))))
    (when actors
      (let ((actor (first actors)))
        (case (envelope-message-type envelope)
          (:tell
           (act:tell actor (deserialize serializer (envelope-message envelope))))
          (:ask-s
           (let ((result (act:ask-s actor (deserialize ...))))
             ;; Send response envelope back
             (transport-send transport
                             (parse-host (envelope-sender-path envelope))
                             (parse-port (envelope-sender-path envelope))
                             (envelope-for-reply envelope (serialize serializer result)))))
          (:ask
           ;; Same as ask-s but async — use tell with sender routing
           ...))))))
```

When target actor is not found, send an error envelope back with `:actor-not-found`. The caller side translates this into `remote-actor-not-found-error`.

### Files Created/Modified

| File | Action |
|------|--------|
| `src/remoting/remoting.lisp` | create |
| `tests/remoting/remoting-test.lisp` | create |
| `sento-remoting.asd` | add remoting module |

### Tests (Phase 5): 10 tests

**remoting-test (~10):**
- `enable-remoting` starts transport listener
- `disable-remoting` stops transport and cleans up
- Config parsing and validation
- Full round-trip: system A `tell` actor on system B
- Full round-trip: system A `ask-s` actor on system B, gets response
- Full round-trip: system A `ask` actor on system B, future resolved
- Bidirectional: actor on system B replies via `*sender*` to system A
- Actor not found -> error envelope returned -> `remote-actor-not-found-error`
- `ask-s` with remote handler error -> returns `(cons :handler-error ...)` consistently
- Expired/invalid certificate -> `tls-certificate-error` on connect attempt

### Done When

- Two actor-systems on localhost communicate over TLS
- `tell`, `ask-s`, `ask` all work end-to-end
- `*sender*` on receiving side is a `remote-actor-ref` pointing back
- Error conditions propagate correctly
- All 10 tests pass
- Cumulative: **53 tests passing**, ~90%+ coverage

---

## Phase 6: Interactive Text Protocol (Telnet-like)

**Goal:** A line-based text protocol for interacting with a running actor system over TLS. Useful for manual debugging, CI smoke tests, and health checks. Also provides a Lisp-side REPL client that reuses the same protocol.

**Prerequisite:** Phase 5 complete.

### Text Protocol Definition

The protocol is line-based, one command per line, one response per command. The server-side handler reads lines, dispatches commands, and writes responses.

```
Commands:
  tell <path> <message>       — fire-and-forget, responds OK or ERROR: ...
  ask <path> <message>        — synchronous, responds with result or ERROR: ...
  actors [<path-prefix>]      — list actors, optional path filter, one path per line, terminated by empty line
  ping                        — responds PONG (health check / connectivity test)
  help                        — lists available commands
  quit                        — close connection

Response format:
  OK                          — success (tell, quit)
  PONG                        — ping response
  <printed-lisp-object>       — ask result
  ERROR: <type>: <message>    — error (e.g. ERROR: actor-not-found: /user/foo)

  actors response:
    /user/greeter
    /user/processor
    <empty line>              — end of list
```

### Architecture (Shared Core)

```
text-protocol-handler (core)           — parses commands, dispatches, formats responses
  |
  +-- Option B: server-side listener   — accepts TLS connections, runs handler per connection
  |                                      external clients connect directly (openssl s_client, etc.)
  |
  +-- Option A: connect-repl          — opens TLS connection to remote system,
                                        wraps stdin/stdout in a REPL loop that
                                        sends text protocol commands and prints responses
```

Both options share the same protocol handler code. Option A is a thin client-side wrapper.

### What to Build

#### 6.1 Text Protocol Handler (`src/remoting/text-protocol.lisp`)

**Package:** `sento.remoting.text-protocol` (nickname `:rtp`)

The core handler that processes a single connection:

```lisp
(defun handle-text-connection (stream system)
  ;; Read lines from stream, dispatch commands, write responses
  ;; Loops until quit or connection closed
  ...)
```

Command dispatch:

```lisp
(defun %dispatch-command (command args system)
  ;; Parse command keyword, dispatch to handler
  ;; Returns response string
  (case command
    (:tell (handler-case
               (let ((actor (first (ac:find-actors system path))))
                 (if actor
                     (progn (act:tell actor (read-from-string message-str))
                            "OK")
                     (format nil "ERROR: actor-not-found: ~a" path)))
             (serious-condition (c)
               (format nil "ERROR: ~a: ~a" (type-of c) c))))
    (:ask  ...)
    (:actors ...)
    (:ping "PONG")
    (:help ...)
    (:quit nil)  ; nil signals end of session
    (t (format nil "ERROR: unknown-command: ~a" command))))
```

#### 6.2 Text Protocol Server (`src/remoting/text-protocol.lisp`)

Listener that accepts connections and runs `handle-text-connection` per client. Shares the TLS config from the remoting setup.

```lisp
(defun start-text-server (system &key host port tls-config)
  ;; Start a TLS listener on a separate port
  ;; Each accepted connection runs handle-text-connection in its own thread
  ;; Returns a text-server instance (for stopping)
  ...)

(defun stop-text-server (text-server)
  ;; Stop listener, close all connections
  ...)
```

Enabled via `enable-remoting` with an optional `:text-port`:

```lisp
(rem:enable-remoting *system*
  :host "0.0.0.0"
  :port 4711
  :text-port 4712          ; text protocol on separate port
  :tls (:certificate "cert.pem"
        :private-key "key.pem"
        :ca-certificate "ca.pem"))
```

When `:text-port` is omitted, no text protocol server is started.

#### 6.3 REPL Client (`src/remoting/text-protocol.lisp`)

Client-side function that connects to a remote text protocol server:

```lisp
(defun connect-repl (uri &key tls-config)
  ;; Parse sento-text://host:port or just host:port
  ;; Open TLS connection
  ;; Loop: read from *standard-input*, send to server, print response
  ;; Until user types quit or EOF
  ...)
```

Usage:

```lisp
;; From any Lisp image
(rtp:connect-repl "localhost:4712"
  :tls-config (rtls:make-tls-config
                :certificate "cert.pem"
                :private-key "key.pem"
                :ca-certificate "ca.pem"))

sento> ping
PONG
sento> actors
/user/greeter
/user/processor

sento> ask /user/greeter "World"
"Hello, World!"
sento> tell /user/greeter "World"
OK
sento> quit
```

Or from the command line with openssl:

```bash
$ openssl s_client -connect localhost:4712 \
    -CAfile ca.pem -cert cert.pem -key key.pem -quiet

ping
PONG
ask /user/greeter "World"
"Hello, World!"
actors
/user/greeter
/user/processor

quit
```

### Files Created/Modified

| File | Action |
|------|--------|
| `src/remoting/text-protocol.lisp` | create (handler, server, REPL client) |
| `tests/remoting/text-protocol-test.lisp` | create |
| `sento-remoting.asd` | add text-protocol module |
| `src/remoting/packages.lisp` | add `sento.remoting.text-protocol` package |
| `src/remoting/remoting.lisp` | extend `enable-remoting` with `:text-port` |

### Tests (Phase 6): 10 tests

**text-protocol-test (~10):**
- `ping` command returns `PONG`
- `help` command returns command list
- `quit` command closes session
- `tell` to existing actor returns `OK`
- `tell` to non-existent actor returns `ERROR: actor-not-found: ...`
- `ask` to existing actor returns printed result
- `ask` to non-existent actor returns `ERROR: actor-not-found: ...`
- `actors` lists all actor paths, terminated by empty line
- `actors` with path prefix filters results
- Unknown command returns `ERROR: unknown-command: ...`

### Done When

- Text protocol server accepts TLS connections and handles commands
- `connect-repl` connects and provides interactive session
- `openssl s_client` can interact with the server
- All 10 tests pass
- Cumulative: **63 tests passing**

---

## Test Coverage Summary

| Phase | Tests | Cumulative | What is Covered |
|-------|-------|-----------|-----------------|
| Phase 1 | 14 | 14 | conditions, serialization, envelope |
| Phase 2 | 8 | 22 | TLS protocol, pure-tls backend, cert errors |
| Phase 3 | 10 | 32 | transport lifecycle, framing, pooling, connection errors |
| Phase 4 | 11 | 43 | remote-ref URI parsing, send queue for tell, direct send for ask-s/ask, timeouts |
| Phase 5 | 10 | 53 | end-to-end integration, bidirectional, error propagation |
| Phase 6 | 10 | **63** | text protocol commands, REPL client, error responses |

---

## Error Handling and Conditions

Remoting introduces network boundaries where many things can go wrong. All error scenarios must surface as typed conditions so that callers can handle them precisely with `handler-case` / `handler-bind`.

### Condition Hierarchy

See Phase 1, section 1.3 for the full condition definitions.

```
remoting-error
+-- serialization-error
+-- deserialization-error
+-- tls-error
|   +-- tls-handshake-error
|   +-- tls-certificate-error
|   +-- tls-peer-verify-error
+-- transport-error
|   +-- connection-refused-error
|   +-- connection-timeout-error
|   +-- connection-closed-error
|   +-- send-failed-error
+-- remote-actor-error
    +-- remote-actor-not-found-error
    +-- invalid-remote-uri-error
```

### Where Conditions Are Signaled

| Condition | Signaled By | When |
|-----------|-------------|------|
| `tls-handshake-error` | `tls-wrap` (Phase 2) | TLS version mismatch, cipher negotiation failure, protocol error |
| `tls-certificate-error` | `tls-wrap` (Phase 2) | Expired cert, untrusted CA, hostname mismatch, self-signed |
| `tls-peer-verify-error` | `tls-wrap` (Phase 2) | mTLS required but peer sent no client cert |
| `connection-refused-error` | `transport-send` (Phase 3) | Remote host not listening or firewall block |
| `connection-timeout-error` | `transport-send` (Phase 3) | TCP connect times out |
| `connection-closed-error` | Transport reader thread (Phase 3) | Remote side drops connection |
| `send-failed-error` | `transport-send` (Phase 3) | Write to socket fails after connect |
| `serialization-error` | `serialize` (Phase 1) | Object type not supported by serializer |
| `deserialization-error` | `deserialize` (Phase 1) | Corrupt or incompatible payload |
| `remote-actor-not-found-error` | `handle-inbound` (Phase 5) | Target path doesn't resolve; returned to caller via error envelope |
| `invalid-remote-uri-error` | `make-remote-ref` (Phase 4) | Malformed `sento://` URI |

### Error Propagation to the Caller

When the caller uses `tell`/`ask-s`/`ask` on a `remote-actor-ref`:

**`tell`** — Fire-and-forget, queued via internal sender actor. The caller never sees transport errors — they are handled by the sender actor's receive function (logged at `log:warn`, future: published as dead letters to the event stream). This is consistent with local `tell` behavior and follows the Erlang/Akka pattern where remote `tell` is best-effort. If the send queue is bounded and full, `queue-full-error` is signaled to the caller (same backpressure as local actors).

**`ask-s`** — Synchronous, direct send (bypasses queue). Transport/TLS errors (`connection-refused-error`, `tls-handshake-error`, etc.) are signaled immediately since the caller is on the stack:

```lisp
(handler-case (act:ask-s remote-ref "hello" :time-out 5)
  (rtls:tls-handshake-error (c)
    (log:error "TLS handshake failed: ~a" c))
  (rtrans:connection-refused-error (c)
    (log:error "Cannot reach remote: ~a" c)))
```

Remote-side errors (actor not found, handler error) are returned as `(cons :handler-error condition)`, consistent with local `ask-s` behavior.

**`ask`** — Asynchronous with future, direct send (bypasses queue). Transport/TLS errors during send are signaled immediately. Remote-side errors are delivered as `(cons :handler-error condition)` in the future result, consistent with local `ask` behavior.

### Logging

All error conditions are logged via `log4cl` at appropriate levels:

- `log:error` — TLS failures, connection failures, deserialization errors
- `log:warn` — connection-closed (may be normal shutdown), actor-not-found
- `log:debug` — successful connections, reconnections
