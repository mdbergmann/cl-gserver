(defpackage :sento.remoting.transport-tcp
  (:use :cl)
  (:nicknames :rtrans-tcp)
  (:import-from :sento.remoting.transport
                #:transport
                #:transport-host
                #:transport-port
                #:transport-tls-config
                #:transport-tls-provider
                #:transport-running-p
                #:transport-start
                #:transport-stop
                #:transport-send
                #:transport-error
                #:connection-refused-error
                #:connection-timeout-error
                #:send-failed-error)
  (:import-from :sento.remoting.envelope
                #:make-envelope
                #:envelope-target-path
                #:envelope-sender-path
                #:envelope-message
                #:envelope-message-type
                #:envelope-correlation-id)
  (:import-from :sento.remoting.tls
                #:tls-wrap
                #:tls-unwrap
                #:tls-error
                #:tls-config-provider
                #:tls-config-certificate
                #:tls-config-private-key
                #:tls-config-ca-certificate
                #:tls-config-peer-verify)
  (:import-from :sento.remoting.tls-pure
                #:pure-tls-provider)
  (:import-from :bordeaux-threads-2
                #:make-lock
                #:with-lock-held
                #:make-thread
                #:thread-alive-p
                #:join-thread)
  (:import-from :usocket
                #:socket-connect
                #:socket-listen
                #:socket-accept
                #:socket-close
                #:socket-stream
                #:get-local-port)
  (:import-from :flexi-streams
                #:string-to-octets
                #:octets-to-string)
  (:export #:tcp-transport
           #:tcp-transport-actual-port))

(in-package :sento.remoting.transport-tcp)

;; ---------------------------------
;; tcp-transport
;; ---------------------------------

(defclass tcp-transport (transport)
  ((listener-socket :initform nil
                    :accessor %listener-socket
                    :documentation "The usocket listener socket.")
   (listener-thread :initform nil
                    :accessor %listener-thread
                    :documentation "Thread running the accept loop.")
   (connections :initform (make-hash-table :test 'equal)
                :reader %connections
                :documentation "Connection pool: \"host:port\" -> connection plist.")
   (connections-lock :initform (make-lock :name "tcp-transport-connections")
                     :reader %connections-lock
                     :documentation "Lock for the connections hash-table.")
   (reader-threads :initform nil
                   :accessor %reader-threads
                   :documentation "List of active reader threads.")
   (actual-port :initform nil
                :accessor tcp-transport-actual-port
                :documentation "Actual port after bind (useful when port=0)."))
  (:documentation "TCP transport with TLS and length-prefixed framing."))

;; ---------------------------------
;; framing: 4-byte big-endian length prefix
;; ---------------------------------

(defun %write-frame (stream payload)
  "Write a length-prefixed frame: 4-byte big-endian length + payload bytes."
  (let ((len (length payload)))
    (write-byte (ldb (byte 8 24) len) stream)
    (write-byte (ldb (byte 8 16) len) stream)
    (write-byte (ldb (byte 8  8) len) stream)
    (write-byte (ldb (byte 8  0) len) stream)
    (write-sequence payload stream)
    (force-output stream)))

(defun %read-frame (stream)
  "Read a length-prefixed frame. Returns payload byte vector, or NIL on EOF."
  (let ((b3 (read-byte stream nil nil)))
    (unless b3 (return-from %read-frame nil))
    (let ((b2 (read-byte stream nil nil))
          (b1 (read-byte stream nil nil))
          (b0 (read-byte stream nil nil)))
      (unless (and b2 b1 b0)
        (return-from %read-frame nil))
      (let* ((len (logior (ash b3 24) (ash b2 16) (ash b1 8) b0))
             (buf (make-array len :element-type '(unsigned-byte 8))))
        (let ((pos 0))
          (loop :while (< pos len)
                :do (let ((n (read-sequence buf stream :start pos :end len)))
                      (when (= n pos)
                        (return-from %read-frame nil))
                      (setf pos n))))
        buf))))

;; ---------------------------------
;; envelope serialization (to/from bytes)
;; ---------------------------------

(defun %serialize-envelope (envelope)
  "Serialize an envelope struct to bytes using sexp representation."
  (let ((sexp (list :target-path (envelope-target-path envelope)
                    :sender-path (envelope-sender-path envelope)
                    :message (when (envelope-message envelope)
                               (coerce (envelope-message envelope) 'list))
                    :message-type (envelope-message-type envelope)
                    :correlation-id (envelope-correlation-id envelope))))
    (string-to-octets
     (write-to-string sexp :readably t)
     :external-format :utf-8)))

(defun %deserialize-envelope (bytes)
  "Deserialize bytes into an envelope struct."
  (let* ((string (octets-to-string bytes :external-format :utf-8))
         (sexp (read-from-string string))
         (msg-list (getf sexp :message)))
    (make-envelope
     :target-path (getf sexp :target-path)
     :sender-path (getf sexp :sender-path)
     :message (when msg-list
                (make-array (length msg-list)
                            :element-type '(unsigned-byte 8)
                            :initial-contents msg-list))
     :message-type (getf sexp :message-type)
     :correlation-id (getf sexp :correlation-id))))

;; ---------------------------------
;; TLS wrapping helpers
;; ---------------------------------

(defun %make-tls-provider (tls-config)
  "Create a TLS provider instance from config."
  (ecase (tls-config-provider tls-config)
    (:pure-tls (make-instance 'pure-tls-provider))))

(defun %wrap-server-stream (transport raw-stream)
  "Wrap a raw stream with TLS for server role. Returns TLS stream or raw stream if no TLS."
  (let ((config (transport-tls-config transport))
        (provider (transport-tls-provider transport)))
    (if (and config provider)
        (tls-wrap provider raw-stream
                       :certificate (tls-config-certificate config)
                       :private-key (tls-config-private-key config)
                       :ca-certificate (tls-config-ca-certificate config)
                       :peer-verify (tls-config-peer-verify config)
                       :role :server)
        raw-stream)))

(defun %wrap-client-stream (transport raw-stream hostname)
  "Wrap a raw stream with TLS for client role. Returns TLS stream or raw stream if no TLS."
  (let ((config (transport-tls-config transport))
        (provider (transport-tls-provider transport)))
    (if (and config provider)
        (tls-wrap provider raw-stream
                       :hostname hostname
                       :certificate (tls-config-certificate config)
                       :private-key (tls-config-private-key config)
                       :ca-certificate (tls-config-ca-certificate config)
                       :peer-verify (tls-config-peer-verify config)
                       :role :client)
        raw-stream)))

;; ---------------------------------
;; connection management
;; ---------------------------------

(defun %connection-key (host port)
  "Create a connection pool key from host and port."
  (format nil "~a:~a" host port))

(defun %get-connection (transport host port)
  "Get or create a connection to host:port. Returns a stream."
  (let ((key (%connection-key host port)))
    (with-lock-held ((%connections-lock transport))
      (let ((entry (gethash key (%connections transport))))
        (when (and entry (open-stream-p (getf entry :stream)))
          (return-from %get-connection (getf entry :stream)))))
    ;; Not found or closed — create new connection
    (let* ((socket (handler-case
                       (socket-connect host port
                                               :element-type '(unsigned-byte 8))
                     (usocket:connection-refused-error ()
                       (error 'connection-refused-error
                              :host host :port port
                              :message (format nil "Connection refused to ~a:~a" host port)))
                     (usocket:timeout-error ()
                       (error 'connection-timeout-error
                              :host host :port port
                              :message (format nil "Connection timed out to ~a:~a" host port)))
                     (usocket:socket-error (c)
                       (error 'connection-refused-error
                              :host host :port port
                              :message (format nil "~a" c)))))
           (raw-stream (socket-stream socket))
           (stream (handler-case
                       (%wrap-client-stream transport raw-stream host)
                     (tls-error (c)
                       (socket-close socket)
                       (error c)))))
      (with-lock-held ((%connections-lock transport))
        (setf (gethash key (%connections transport))
              (list :stream stream :socket socket)))
      stream)))

(defun %remove-connection (transport host port)
  "Remove and close a cached connection."
  (let ((key (%connection-key host port)))
    (with-lock-held ((%connections-lock transport))
      (let ((entry (gethash key (%connections transport))))
        (when entry
          (remhash key (%connections transport))
          (handler-case
              (progn
                (when (transport-tls-provider transport)
                  (tls-unwrap (transport-tls-provider transport)
                                   (getf entry :stream)))
                (socket-close (getf entry :socket)))
            (error (c)
              (log:debug "Error closing connection ~a: ~a" key c))))))))

(defun %close-all-connections (transport)
  "Close all cached outbound connections."
  (with-lock-held ((%connections-lock transport))
    (maphash (lambda (key entry)
               (declare (ignore key))
               (handler-case
                   (progn
                     (when (transport-tls-provider transport)
                       (tls-unwrap (transport-tls-provider transport)
                                        (getf entry :stream)))
                     (socket-close (getf entry :socket)))
                 (error (c)
                   (log:debug "Error closing connection: ~a" c))))
             (%connections transport))
    (clrhash (%connections transport))))

;; ---------------------------------
;; reader thread (per accepted connection)
;; ---------------------------------

(defun %start-reader-thread (transport stream socket)
  "Start a reader thread that reads frames from STREAM and dispatches envelopes."
  (let ((thread
          (make-thread
           (lambda ()
             (unwind-protect
                  (handler-case
                      (loop :for frame = (%read-frame stream)
                            :while (and frame (transport-running-p transport))
                            :do (handler-case
                                    (let ((envelope (%deserialize-envelope frame)))
                                      (let ((handler (rtrans::%transport-message-handler transport)))
                                        (when handler
                                          (funcall handler envelope))))
                                  (error (c)
                                    (log:warn "Error processing inbound frame: ~a" c))))
                    (end-of-file ()
                      (log:debug "Reader: connection closed (EOF)."))
                    (error (c)
                      (log:debug "Reader: connection error: ~a" c)))
               (handler-case
                   (progn
                     (when (transport-tls-provider transport)
                       (tls-unwrap (transport-tls-provider transport) stream))
                     (socket-close socket))
                 (error (c)
                   (log:debug "Error cleaning up reader connection: ~a" c)))))
           :name "tcp-transport-reader")))
    (push thread (%reader-threads transport))
    thread))

;; ---------------------------------
;; listener (accept loop)
;; ---------------------------------

(defun %accept-loop (transport)
  "Accept loop: accept connections, wrap with TLS, start reader threads."
  (loop :while (transport-running-p transport)
        :do (handler-case
                (let ((client-socket
                        (socket-accept (%listener-socket transport)
                                               :element-type '(unsigned-byte 8))))
                  (when client-socket
                    (handler-case
                        (let ((stream (%wrap-server-stream
                                       transport
                                       (socket-stream client-socket))))
                          (%start-reader-thread transport stream client-socket))
                      (error (c)
                        (log:warn "Error wrapping accepted connection with TLS: ~a" c)
                        (handler-case (socket-close client-socket)
                          (error () nil))))))
              (usocket:socket-error ()
                ;; Listener socket closed during shutdown
                (return))
              (error (c)
                (when (transport-running-p transport)
                  (log:warn "Accept loop error: ~a" c))))))

;; ---------------------------------
;; transport protocol implementation
;; ---------------------------------

(defmethod initialize-instance :after ((transport tcp-transport) &key)
  (let ((config (transport-tls-config transport)))
    (when (and config (not (transport-tls-provider transport)))
      (setf (transport-tls-provider transport)
            (%make-tls-provider config)))))

(defmethod transport-start ((transport tcp-transport) message-handler-fn)
  (when (transport-running-p transport)
    (error "Transport is already running."))
  (setf (rtrans::%transport-message-handler transport) message-handler-fn)
  (setf (transport-running-p transport) t)
  (let ((socket (socket-listen (transport-host transport)
                                       (transport-port transport)
                                       :reuse-address t
                                       :element-type '(unsigned-byte 8)
                                       :backlog 5)))
    (setf (%listener-socket transport) socket)
    (setf (tcp-transport-actual-port transport)
          (get-local-port socket))
    (setf (%listener-thread transport)
          (make-thread
           (lambda () (%accept-loop transport))
           :name "tcp-transport-listener")))
  transport)

(defmethod transport-stop ((transport tcp-transport))
  (when (transport-running-p transport)
    (setf (transport-running-p transport) nil)
    ;; Close listener socket to unblock accept
    (when (%listener-socket transport)
      (handler-case (socket-close (%listener-socket transport))
        (error () nil))
      (setf (%listener-socket transport) nil))
    ;; Wait for listener thread
    (when (and (%listener-thread transport)
               (thread-alive-p (%listener-thread transport)))
      (join-thread (%listener-thread transport)))
    (setf (%listener-thread transport) nil)
    ;; Wait for reader threads
    (dolist (thread (%reader-threads transport))
      (when (thread-alive-p thread)
        (handler-case (join-thread thread)
          (error () nil))))
    (setf (%reader-threads transport) nil)
    ;; Close all outbound connections
    (%close-all-connections transport))
  transport)

(defmethod transport-send ((transport tcp-transport) target-host target-port envelope)
  (unless (transport-running-p transport)
    (error 'send-failed-error
           :envelope envelope
           :message "Transport is not running."))
  (let ((stream (handler-case
                    (%get-connection transport target-host target-port)
                  (transport-error (c)
                    (error c)))))
    (handler-case
        (let ((frame (%serialize-envelope envelope)))
          (%write-frame stream frame))
      (error (c)
        ;; Remove broken connection so next attempt reconnects
        (%remove-connection transport target-host target-port)
        (if (typep c 'transport-error)
            (error c)
            (error 'send-failed-error
                   :envelope envelope
                   :message (format nil "~a" c)))))))

