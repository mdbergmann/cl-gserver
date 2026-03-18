(defpackage :sento.remoting.transport
  (:use :cl)
  (:nicknames :rtrans)
  (:import-from :sento.remoting
                #:remoting-error
                #:remoting-error-message)
  (:export #:transport
           #:transport-host
           #:transport-port
           #:transport-tls-config
           #:transport-tls-provider
           #:transport-running-p
           #:transport-message-handler
           ;; protocol
           #:transport-start
           #:transport-stop
           #:transport-send
           #:transport-max-message-length
           ;; conditions
           #:transport-error
           #:message-too-large-error
           #:message-too-large-size
           #:message-too-large-max
           #:connection-refused-error
           #:connection-refused-host
           #:connection-refused-port
           #:connection-timeout-error
           #:connection-timeout-host
           #:connection-timeout-port
           #:connection-closed-error
           #:send-failed-error
           #:send-failed-envelope))

(in-package :sento.remoting.transport)

;; ---------------------------------
;; conditions
;; ---------------------------------

(define-condition transport-error (remoting-error)
  ()
  (:documentation "Base condition for transport-related errors."))

(define-condition message-too-large-error (transport-error)
  ((size :initarg :size
         :reader message-too-large-size
         :documentation "The actual message size in bytes.")
   (max :initarg :max
        :reader message-too-large-max
        :documentation "The maximum allowed size in bytes."))
  (:report (lambda (c stream)
             (format stream "Message too large: ~a bytes (max ~a)"
                     (message-too-large-size c) (message-too-large-max c))))
  (:documentation "Signaled when a message exceeds the configured maximum size."))

(define-condition connection-refused-error (transport-error)
  ((host :initarg :host
         :reader connection-refused-host
         :documentation "The host that refused the connection.")
   (port :initarg :port
         :reader connection-refused-port
         :documentation "The port that refused the connection."))
  (:report (lambda (c stream)
             (format stream "Connection refused to ~a:~a"
                     (connection-refused-host c) (connection-refused-port c))))
  (:documentation "Signaled when TCP connection to remote host is refused."))

(define-condition connection-timeout-error (transport-error)
  ((host :initarg :host
         :reader connection-timeout-host
         :documentation "The host that timed out.")
   (port :initarg :port
         :reader connection-timeout-port
         :documentation "The port that timed out."))
  (:report (lambda (c stream)
             (format stream "Connection timed out to ~a:~a"
                     (connection-timeout-host c) (connection-timeout-port c))))
  (:documentation "Signaled when TCP connection attempt times out."))

(define-condition connection-closed-error (transport-error)
  ()
  (:report (lambda (c stream)
             (format stream "Connection closed by remote side: ~a"
                     (remoting-error-message c))))
  (:documentation "Signaled when an established connection is unexpectedly closed by the remote side."))

(define-condition send-failed-error (transport-error)
  ((envelope :initarg :envelope
             :reader send-failed-envelope
             :initform nil
             :documentation "The envelope that failed to send."))
  (:report (lambda (c stream)
             (format stream "Send failed: ~a" (remoting-error-message c))))
  (:documentation "Signaled when sending an envelope fails after connection was established."))

;; ---------------------------------
;; abstract base class
;; ---------------------------------

(defclass transport ()
  ((host :initarg :host
         :reader transport-host
         :initform "0.0.0.0"
         :documentation "Host/interface to bind the listener to.")
   (port :initarg :port
         :reader transport-port
         :initform 0
         :documentation "Port to bind the listener to. 0 means auto-assign.")
   (tls-config :initarg :tls-config
               :reader transport-tls-config
               :initform nil
               :documentation "TLS configuration for this transport.")
   (tls-provider :initarg :tls-provider
                 :accessor transport-tls-provider
                 :initform nil
                 :documentation "TLS provider instance for this transport.")
   (message-handler :initarg :message-handler
                    :accessor transport-message-handler
                    :initform nil
                    :documentation "Function called with (envelope) for inbound messages.")
   (max-message-length :initarg :max-message-length
                       :reader transport-max-message-length
                       :initform (* 2 1024 1024)
                       :documentation "Maximum message size in bytes. Default 2MB.")
   (running-p :initform nil
              :accessor transport-running-p
              :documentation "Whether this transport is currently running."))
  (:documentation "Abstract base class for remoting transports.
Subclasses implement the actual connection and framing logic."))

;; ---------------------------------
;; protocol (generic functions)
;; ---------------------------------

(defgeneric transport-start (transport message-handler-fn)
  (:documentation "Start listening for inbound connections.
MESSAGE-HANDLER-FN is called with each received envelope."))

(defgeneric transport-stop (transport)
  (:documentation "Stop listener and close all connections."))

(defgeneric transport-send (transport target-host target-port envelope)
  (:documentation "Send an envelope to a remote host. Manages connections internally."))
