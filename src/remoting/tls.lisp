(defpackage :sento.remoting.tls
  (:use :cl)
  (:nicknames :rtls)
  (:export #:tls-wrap
           #:tls-unwrap
           ;; config
           #:tls-config
           #:make-tls-config
           #:tls-config-provider
           #:tls-config-certificate
           #:tls-config-private-key
           #:tls-config-ca-certificate
           #:tls-config-peer-verify
           ;; conditions
           #:tls-error
           #:tls-handshake-error
           #:tls-handshake-error-reason
           #:tls-certificate-error
           #:tls-certificate-error-reason
           #:tls-certificate-error-subject
           #:tls-peer-verify-error))

(in-package :sento.remoting.tls)

;; ---------------------------------
;; conditions
;; ---------------------------------

(define-condition tls-error (rem:remoting-error)
  ()
  (:documentation "Base condition for TLS-related errors."))

(define-condition tls-handshake-error (tls-error)
  ((reason :initarg :reason
           :reader tls-handshake-error-reason
           :initform nil
           :documentation "Reason for handshake failure."))
  (:report (lambda (c stream)
             (format stream "TLS handshake failed: ~a"
                     (tls-handshake-error-reason c))))
  (:documentation "Signaled when TLS handshake fails."))

(define-condition tls-certificate-error (tls-error)
  ((reason :initarg :reason
           :reader tls-certificate-error-reason
           :initform nil
           :documentation "Reason for certificate failure.")
   (certificate-subject :initarg :certificate-subject
                        :reader tls-certificate-error-subject
                        :initform nil
                        :documentation "Subject of the offending certificate."))
  (:report (lambda (c stream)
             (format stream "TLS certificate error: ~a (subject: ~a)"
                     (tls-certificate-error-reason c)
                     (tls-certificate-error-subject c))))
  (:documentation "Signaled on certificate validation failure."))

(define-condition tls-peer-verify-error (tls-error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "TLS peer verification failed: peer did not present a certificate")))
  (:documentation "Signaled when mutual TLS is required but peer did not present a client certificate."))

;; ---------------------------------
;; configuration
;; ---------------------------------

(defstruct tls-config
  "TLS configuration for remoting connections."
  (provider :pure-tls
   :type keyword)
  (certificate nil
   :type (or null string pathname))
  (private-key nil
   :type (or null string pathname))
  (ca-certificate nil
   :type (or null string pathname))
  (peer-verify t
   :type boolean))

;; ---------------------------------
;; protocol
;; ---------------------------------

(defgeneric tls-wrap (provider stream &key certificate private-key
                                           ca-certificate hostname
                                           peer-verify role)
  (:documentation "Wrap a raw STREAM with TLS using PROVIDER.
ROLE is :server or :client.
Returns a TLS-wrapped stream.
Signals tls-handshake-error, tls-certificate-error, or tls-peer-verify-error on failure."))

(defgeneric tls-unwrap (provider wrapped-stream)
  (:documentation "Unwrap a TLS stream, performing graceful TLS shutdown."))
