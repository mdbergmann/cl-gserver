(defpackage :sento.remoting.tls-pure
  (:use :cl)
  (:export #:pure-tls-provider))

(in-package :sento.remoting.tls-pure)

;; ---------------------------------
;; pure-tls backend
;; ---------------------------------

(defclass pure-tls-provider ()
  ()
  (:documentation "TLS provider implementation using the pure-tls library.
Supports TLS 1.3 with mutual authentication."))

(defmethod rtls:tls-wrap ((provider pure-tls-provider) stream
                           &key certificate private-key ca-certificate
                                hostname peer-verify role)
  (let ((verify-mode (if peer-verify
                         pure-tls:+verify-required+
                         pure-tls:+verify-none+)))
    (handler-case
        (ecase role
          (:server
           (%tls-wrap-server stream certificate private-key
                             ca-certificate verify-mode))
          (:client
           (%tls-wrap-client stream certificate private-key
                             ca-certificate hostname verify-mode)))
      (pure-tls:tls-verification-error (c)
        (error 'rtls:tls-certificate-error
               :reason (format nil "~a" c)
               :message (format nil "~a" c)))
      (pure-tls:tls-certificate-error (c)
        (error 'rtls:tls-certificate-error
               :reason (format nil "~a" c)
               :message (format nil "~a" c)))
      (pure-tls:tls-handshake-error (c)
        (error 'rtls:tls-handshake-error
               :reason (format nil "~a" c)
               :message (format nil "~a" c)))
      (pure-tls:tls-error (c)
        (error 'rtls:tls-handshake-error
               :reason (format nil "~a" c)
               :message (format nil "~a" c))))))

(defmethod rtls:tls-unwrap ((provider pure-tls-provider) wrapped-stream)
  (when wrapped-stream
    (handler-case
        (close wrapped-stream)
      (error (c)
        (log:debug "Error during TLS unwrap: ~a" c)))))

;; ---------------------------------
;; internal helpers
;; ---------------------------------

(defun %tls-wrap-server (stream certificate private-key
                         ca-certificate verify-mode)
  "Wrap STREAM as a TLS server."
  (let ((pure-tls:*use-macos-keychain* nil)
        (context (pure-tls:make-tls-context
                  :verify-mode verify-mode
                  :auto-load-system-ca nil))
        (trust-store (when ca-certificate
                       (pure-tls::make-trust-store
                        :certificates (pure-tls:load-certificate-chain
                                       (namestring ca-certificate))))))
    (pure-tls:make-tls-server-stream
     stream
     :context context
     :certificate (namestring certificate)
     :key (namestring private-key)
     :verify verify-mode
     :trust-store trust-store)))

(defun %tls-wrap-client (stream certificate private-key
                         ca-certificate hostname verify-mode)
  "Wrap STREAM as a TLS client."
  (let ((pure-tls:*use-macos-keychain* nil)
        (context (pure-tls:make-tls-context
                  :verify-mode verify-mode
                  :ca-file (when ca-certificate
                             (namestring ca-certificate))
                  :auto-load-system-ca nil)))
    (pure-tls:make-tls-client-stream
     stream
     :hostname hostname
     :context context
     :verify verify-mode
     :client-certificate (when certificate (namestring certificate))
     :client-key (when private-key (namestring private-key)))))
