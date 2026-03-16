(defpackage :sento.remoting.transport-tcp
  (:use :cl)
  (:nicknames :rtrans-tcp)
  (:export #:tcp-transport
           #:tcp-transport-actual-port))

(in-package :sento.remoting.transport-tcp)

;; ---------------------------------
;; tcp-transport
;; ---------------------------------

(defclass tcp-transport (rtrans:transport)
  ((listener-socket :initform nil
                    :accessor %listener-socket
                    :documentation "The usocket listener socket.")
   (listener-thread :initform nil
                    :accessor %listener-thread
                    :documentation "Thread running the accept loop.")
   (connections :initform (make-hash-table :test 'equal)
                :reader %connections
                :documentation "Connection pool: \"host:port\" -> connection plist.")
   (connections-lock :initform (bt2:make-lock :name "tcp-transport-connections")
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
  (let ((sexp (list :target-path (renv:envelope-target-path envelope)
                    :sender-path (renv:envelope-sender-path envelope)
                    :message (when (renv:envelope-message envelope)
                               (coerce (renv:envelope-message envelope) 'list))
                    :message-type (renv:envelope-message-type envelope)
                    :correlation-id (renv:envelope-correlation-id envelope))))
    (flexi-streams:string-to-octets
     (write-to-string sexp :readably t)
     :external-format :utf-8)))

(defun %deserialize-envelope (bytes)
  "Deserialize bytes into an envelope struct."
  (let* ((string (flexi-streams:octets-to-string bytes :external-format :utf-8))
         (sexp (read-from-string string))
         (msg-list (getf sexp :message)))
    (renv:make-envelope
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
  (ecase (rtls:tls-config-provider tls-config)
    (:pure-tls (make-instance 'sento.remoting.tls-pure:pure-tls-provider))))

(defun %wrap-server-stream (transport raw-stream)
  "Wrap a raw stream with TLS for server role. Returns TLS stream or raw stream if no TLS."
  (let ((config (rtrans:transport-tls-config transport))
        (provider (rtrans:transport-tls-provider transport)))
    (if (and config provider)
        (rtls:tls-wrap provider raw-stream
                       :certificate (rtls:tls-config-certificate config)
                       :private-key (rtls:tls-config-private-key config)
                       :ca-certificate (rtls:tls-config-ca-certificate config)
                       :peer-verify (rtls:tls-config-peer-verify config)
                       :role :server)
        raw-stream)))

(defun %wrap-client-stream (transport raw-stream hostname)
  "Wrap a raw stream with TLS for client role. Returns TLS stream or raw stream if no TLS."
  (let ((config (rtrans:transport-tls-config transport))
        (provider (rtrans:transport-tls-provider transport)))
    (if (and config provider)
        (rtls:tls-wrap provider raw-stream
                       :hostname hostname
                       :certificate (rtls:tls-config-certificate config)
                       :private-key (rtls:tls-config-private-key config)
                       :ca-certificate (rtls:tls-config-ca-certificate config)
                       :peer-verify (rtls:tls-config-peer-verify config)
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
    (bt2:with-lock-held ((%connections-lock transport))
      (let ((entry (gethash key (%connections transport))))
        (when (and entry (open-stream-p (getf entry :stream)))
          (return-from %get-connection (getf entry :stream)))))
    ;; Not found or closed — create new connection
    (let* ((socket (handler-case
                       (usocket:socket-connect host port
                                               :element-type '(unsigned-byte 8))
                     (usocket:connection-refused-error ()
                       (error 'rtrans:connection-refused-error
                              :host host :port port
                              :message (format nil "Connection refused to ~a:~a" host port)))
                     (usocket:timeout-error ()
                       (error 'rtrans:connection-timeout-error
                              :host host :port port
                              :message (format nil "Connection timed out to ~a:~a" host port)))
                     (usocket:socket-error (c)
                       (error 'rtrans:connection-refused-error
                              :host host :port port
                              :message (format nil "~a" c)))))
           (raw-stream (usocket:socket-stream socket))
           (stream (handler-case
                       (%wrap-client-stream transport raw-stream host)
                     (rtls:tls-error (c)
                       (usocket:socket-close socket)
                       (error c)))))
      (bt2:with-lock-held ((%connections-lock transport))
        (setf (gethash key (%connections transport))
              (list :stream stream :socket socket)))
      stream)))

(defun %remove-connection (transport host port)
  "Remove and close a cached connection."
  (let ((key (%connection-key host port)))
    (bt2:with-lock-held ((%connections-lock transport))
      (let ((entry (gethash key (%connections transport))))
        (when entry
          (remhash key (%connections transport))
          (handler-case
              (progn
                (when (rtrans:transport-tls-provider transport)
                  (rtls:tls-unwrap (rtrans:transport-tls-provider transport)
                                   (getf entry :stream)))
                (usocket:socket-close (getf entry :socket)))
            (error (c)
              (log:debug "Error closing connection ~a: ~a" key c))))))))

(defun %close-all-connections (transport)
  "Close all cached outbound connections."
  (bt2:with-lock-held ((%connections-lock transport))
    (maphash (lambda (key entry)
               (declare (ignore key))
               (handler-case
                   (progn
                     (when (rtrans:transport-tls-provider transport)
                       (rtls:tls-unwrap (rtrans:transport-tls-provider transport)
                                        (getf entry :stream)))
                     (usocket:socket-close (getf entry :socket)))
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
          (bt2:make-thread
           (lambda ()
             (unwind-protect
                  (handler-case
                      (loop :for frame = (%read-frame stream)
                            :while (and frame (rtrans:transport-running-p transport))
                            :do (handler-case
                                    (let ((envelope (%deserialize-envelope frame)))
                                      (let ((handler (rtrans:%transport-message-handler transport)))
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
                     (when (rtrans:transport-tls-provider transport)
                       (rtls:tls-unwrap (rtrans:transport-tls-provider transport) stream))
                     (usocket:socket-close socket))
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
  (loop :while (rtrans:transport-running-p transport)
        :do (handler-case
                (let ((client-socket
                        (usocket:socket-accept (%listener-socket transport)
                                               :element-type '(unsigned-byte 8))))
                  (when client-socket
                    (handler-case
                        (let ((stream (%wrap-server-stream
                                       transport
                                       (usocket:socket-stream client-socket))))
                          (%start-reader-thread transport stream client-socket))
                      (error (c)
                        (log:warn "Error wrapping accepted connection with TLS: ~a" c)
                        (handler-case (usocket:socket-close client-socket)
                          (error () nil))))))
              (usocket:socket-error ()
                ;; Listener socket closed during shutdown
                (return))
              (error (c)
                (when (rtrans:transport-running-p transport)
                  (log:warn "Accept loop error: ~a" c))))))

;; ---------------------------------
;; transport protocol implementation
;; ---------------------------------

(defmethod initialize-instance :after ((transport tcp-transport) &key)
  (let ((config (rtrans:transport-tls-config transport)))
    (when (and config (not (rtrans:transport-tls-provider transport)))
      (setf (rtrans:transport-tls-provider transport)
            (%make-tls-provider config)))))

(defmethod rtrans:transport-start ((transport tcp-transport) message-handler-fn)
  (when (rtrans:transport-running-p transport)
    (error "Transport is already running."))
  (setf (rtrans:%transport-message-handler transport) message-handler-fn)
  (setf (rtrans:transport-running-p transport) t)
  (let ((socket (usocket:socket-listen (rtrans:transport-host transport)
                                       (rtrans:transport-port transport)
                                       :reuse-address t
                                       :element-type '(unsigned-byte 8)
                                       :backlog 5)))
    (setf (%listener-socket transport) socket)
    (setf (tcp-transport-actual-port transport)
          (usocket:get-local-port socket))
    (setf (%listener-thread transport)
          (bt2:make-thread
           (lambda () (%accept-loop transport))
           :name "tcp-transport-listener")))
  transport)

(defmethod rtrans:transport-stop ((transport tcp-transport))
  (when (rtrans:transport-running-p transport)
    (setf (rtrans:transport-running-p transport) nil)
    ;; Close listener socket to unblock accept
    (when (%listener-socket transport)
      (handler-case (usocket:socket-close (%listener-socket transport))
        (error () nil))
      (setf (%listener-socket transport) nil))
    ;; Wait for listener thread
    (when (and (%listener-thread transport)
               (bt2:thread-alive-p (%listener-thread transport)))
      (bt2:join-thread (%listener-thread transport)))
    (setf (%listener-thread transport) nil)
    ;; Wait for reader threads
    (dolist (thread (%reader-threads transport))
      (when (bt2:thread-alive-p thread)
        (handler-case (bt2:join-thread thread)
          (error () nil))))
    (setf (%reader-threads transport) nil)
    ;; Close all outbound connections
    (%close-all-connections transport))
  transport)

(defmethod rtrans:transport-send ((transport tcp-transport) target-host target-port envelope)
  (unless (rtrans:transport-running-p transport)
    (error 'rtrans:send-failed-error
           :envelope envelope
           :message "Transport is not running."))
  (let ((stream (handler-case
                    (%get-connection transport target-host target-port)
                  (rtrans:transport-error (c)
                    (error c)))))
    (handler-case
        (let ((frame (%serialize-envelope envelope)))
          (%write-frame stream frame))
      (error (c)
        ;; Remove broken connection so next attempt reconnects
        (%remove-connection transport target-host target-port)
        (if (typep c 'rtrans:transport-error)
            (error c)
            (error 'rtrans:send-failed-error
                   :envelope envelope
                   :message (format nil "~a" c)))))))

