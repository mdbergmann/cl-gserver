(in-package :sento.remoting)

(eval-when (:compile-toplevel)
  (shadowing-import '(bt2:make-lock
                      bt2:with-lock-held
                      renv:envelope-target-path
                      renv:envelope-sender-path
                      renv:envelope-message
                      renv:envelope-message-type
                      renv:envelope-correlation-id
                      renv:envelope-for-reply
                      rseri:sexp-serializer
                      rseri:serialize
                      rseri:deserialize
                      rtrans:transport-start
                      rtrans:transport-stop
                      rtrans:transport-send
                      rtrans-tcp:tcp-transport
                      rtrans-tcp:tcp-transport-actual-port
                      rref:handle-response
                      rref:pending-asks
                      rref:pending-asks-lock
                      rref:stop-sender-actor
                      rref:parse-remote-uri
                      act:tell
                      act:ask-s
                      ac:find-actors
                      str:starts-with-p)))

;; ---------------------------------
;; remoting context
;; ---------------------------------

(defclass remoting-context ()
  ((system :initarg :system
           :reader ctx-system
           :documentation "The actor-system this remoting context belongs to.")
   (transport :initarg :transport
              :reader ctx-transport
              :documentation "The TCP transport for inbound/outbound messages.")
   (serializer :initarg :serializer
               :reader ctx-serializer
               :documentation "The serializer for message encoding/decoding.")
   (hostname :initarg :hostname
             :reader ctx-hostname
             :documentation "The advertised hostname for sento:// URIs.")
   (actual-port :initform nil
                :accessor ctx-actual-port
                :documentation "The actual port after transport starts (for port=0).")
   (remote-refs :initform nil
                :accessor ctx-remote-refs
                :documentation "List of active remote-actor-ref instances.")
   (refs-lock :initform (make-lock :name "remoting-refs-lock")
              :reader ctx-refs-lock
              :documentation "Lock for the remote-refs list."))
  (:documentation "Holds the state for a remoting-enabled actor-system."))

;; ---------------------------------
;; system-to-context mapping
;; ---------------------------------

(defvar *system-remoting* (make-hash-table :test 'eq)
  "Mapping from actor-system to remoting-context.")

(defvar *system-remoting-lock* (make-lock :name "system-remoting-lock")
  "Lock for the system-to-context mapping.")

(defun %get-remoting-context (system)
  "Get the remoting context for a system, or NIL."
  (with-lock-held (*system-remoting-lock*)
    (gethash system *system-remoting*)))

(defun %set-remoting-context (system ctx)
  "Associate a remoting context with a system."
  (with-lock-held (*system-remoting-lock*)
    (setf (gethash system *system-remoting*) ctx)))

(defun %remove-remoting-context (system)
  "Remove the remoting context for a system."
  (with-lock-held (*system-remoting-lock*)
    (remhash system *system-remoting*)))

;; ---------------------------------
;; helpers
;; ---------------------------------

(defun %local-response-path (ctx)
  "Build the local sento:// response path for this remoting context."
  (format nil "sento://~a:~a/__responses__"
          (ctx-hostname ctx)
          (ctx-actual-port ctx)))

;; ---------------------------------
;; inbound message handling
;; ---------------------------------

(defun %handle-inbound (ctx envelope)
  "Main inbound message handler. Routes to local actors or response handling."
  (let ((target-path (envelope-target-path envelope))
        (corr-id (envelope-correlation-id envelope)))
    (cond
      ;; Response to a pending ask: target-path is a sento:// URI
      ((and corr-id (starts-with-p "sento://" target-path))
       (%route-response ctx corr-id envelope))
      ;; Inbound tell
      ((eq :tell (envelope-message-type envelope))
       (%handle-inbound-tell ctx envelope))
      ;; Inbound ask-s or ask
      ((member (envelope-message-type envelope) '(:ask-s :ask))
       (%handle-inbound-ask ctx envelope))
      (t
       (log:warn "Unknown inbound envelope type: ~a for ~a"
                 (envelope-message-type envelope) target-path)))))

(defun %route-response (ctx corr-id envelope)
  "Route a response envelope to the correct remote-ref by correlation-id."
  (let ((matched-ref nil))
    (with-lock-held ((ctx-refs-lock ctx))
      (dolist (ref (ctx-remote-refs ctx))
        (with-lock-held ((pending-asks-lock ref))
          (when (gethash corr-id (pending-asks ref))
            (setf matched-ref ref)
            (return)))))
    (if matched-ref
        (handle-response matched-ref envelope)
        (log:warn "No remote-ref found for response correlation-id ~a, ignoring."
                  corr-id))))

(defun %handle-inbound-tell (ctx envelope)
  "Handle an inbound tell message by routing to the local actor."
  (let* ((system (ctx-system ctx))
         (serializer (ctx-serializer ctx))
         (target-path (envelope-target-path envelope))
         (actors (find-actors system target-path)))
    (if (null actors)
        (log:warn "No actor found for tell to ~a, dropping message." target-path)
        (let ((actor (first actors))
              (message (deserialize serializer (envelope-message envelope))))
          (tell actor message)))))

(defun %handle-inbound-ask (ctx envelope)
  "Handle an inbound ask-s/ask by routing to the local actor and sending the response back."
  (let* ((system (ctx-system ctx))
         (serializer (ctx-serializer ctx))
         (transport (ctx-transport ctx))
         (target-path (envelope-target-path envelope))
         (actors (find-actors system target-path)))
    (if (null actors)
        (log:warn "No actor found for ask to ~a, dropping message." target-path)
        (let* ((actor (first actors))
               (message (deserialize serializer (envelope-message envelope)))
               (result (ask-s actor message :time-out 5))
               (response (envelope-for-reply
                          envelope
                          (serialize serializer result))))
          ;; Parse sender-path to get the response destination
          (handler-case
              (multiple-value-bind (host port path)
                  (parse-remote-uri (envelope-sender-path envelope))
                (declare (ignore path))
                (handler-case
                    (transport-send transport host port response)
                  (error (c)
                    (log:warn "Failed to send response for ~a: ~a" target-path c))))
            (rref:invalid-remote-uri-error (c)
              (log:warn "Cannot parse sender-path for response routing: ~a" c)))))))

;; ---------------------------------
;; public API
;; ---------------------------------

(defun enable-remoting (system &key (host "0.0.0.0") (port 0) (hostname nil) tls-config)
  "Enable remoting on an actor-system.
Starts a TCP transport listener for inbound messages and allows creating remote-refs.

HOST is the interface to bind to (default \"0.0.0.0\").
PORT is the listen port (default 0 for auto-assign).
HOSTNAME is the advertised hostname for response routing (defaults to HOST).
TLS-CONFIG is an optional `rtls:tls-config` for TLS encryption."
  (when (%get-remoting-context system)
    (error 'remoting-error :message "Remoting is already enabled on this system."))
  (let* ((effective-hostname (or hostname host))
         (serializer (make-instance 'sexp-serializer))
         (transport (make-instance 'tcp-transport
                                   :host host
                                   :port port
                                   :tls-config tls-config))
         (ctx (make-instance 'remoting-context
                             :system system
                             :transport transport
                             :serializer serializer
                             :hostname effective-hostname)))
    (transport-start transport
                     (lambda (envelope)
                       (%handle-inbound ctx envelope)))
    (setf (ctx-actual-port ctx) (tcp-transport-actual-port transport))
    (%set-remoting-context system ctx)
    (log:info "Remoting enabled on ~a:~a" effective-hostname (ctx-actual-port ctx))
    ctx))

(defun disable-remoting (system)
  "Disable remoting on an actor-system.
Stops all sender actors, stops the transport, and cleans up."
  (let ((ctx (%get-remoting-context system)))
    (unless ctx
      (return-from disable-remoting nil))
    ;; Stop all sender actors
    (with-lock-held ((ctx-refs-lock ctx))
      (dolist (ref (ctx-remote-refs ctx))
        (handler-case
            (stop-sender-actor ref)
          (error (c)
            (log:debug "Error stopping sender actor: ~a" c))))
      (setf (ctx-remote-refs ctx) nil))
    ;; Stop transport
    (handler-case
        (transport-stop (ctx-transport ctx))
      (error (c)
        (log:warn "Error stopping transport: ~a" c)))
    (%remove-remoting-context system)
    (log:info "Remoting disabled.")
    t))

(defun remoting-enabled-p (system)
  "Returns T if remoting is enabled on the given actor-system."
  (not (null (%get-remoting-context system))))

(defun remoting-port (system)
  "Returns the actual port the remoting transport is listening on, or NIL."
  (let ((ctx (%get-remoting-context system)))
    (when ctx
      (ctx-actual-port ctx))))

(defun make-remote-ref (system uri &key max-queue-size dispatcher)
  "Create a remote-actor-ref for the given sento:// URI.
Requires remoting to be enabled on SYSTEM via `enable-remoting`.
The transport and serializer are provided by the remoting context.
The sender-path for ask-s/ask is set to the local system's sento:// address."
  (let ((ctx (%get-remoting-context system)))
    (unless ctx
      (error 'remoting-error :message "Remoting is not enabled. Call enable-remoting first."))
    (let ((ref (rref:make-remote-ref system uri
                                     (ctx-transport ctx)
                                     (ctx-serializer ctx)
                                     :max-queue-size max-queue-size
                                     :dispatcher dispatcher
                                     :local-sender-path (%local-response-path ctx))))
      (with-lock-held ((ctx-refs-lock ctx))
        (push ref (ctx-remote-refs ctx)))
      ref)))
