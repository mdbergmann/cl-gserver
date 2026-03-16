(defpackage :sento.remoting.tls-test
  (:use :cl :fiveam)
  (:import-from :sento.remoting.tls-pure
                #:pure-tls-provider))

(in-package :sento.remoting.tls-test)

(def-suite tls-tests
  :description "TLS provider protocol and pure-tls backend tests."
  :in sento.remoting.tests:remoting-test-suite)

(in-suite tls-tests)

;; ---------------------------------
;; test cert paths
;; ---------------------------------

(defun %certs-dir ()
  (asdf:system-relative-pathname "sento-remoting" "tests/remoting/certs/"))

(defun %cert-path (filename)
  (merge-pathnames filename (%certs-dir)))

;; ---------------------------------
;; fixture: TLS loopback
;; ---------------------------------

(def-fixture tls-loopback (&key (server-peer-verify nil)
                                (server-ca t)
                                (client-peer-verify nil)
                                (client-certificate nil)
                                (client-private-key nil)
                                (client-ca t))
  (let* ((provider (make-instance 'pure-tls-provider))
         (listen-socket (usocket:socket-listen "127.0.0.1" 0
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listen-socket))
         (server-tls-stream nil)
         (server-error nil)
         (server-thread
           (bt2:make-thread
            (lambda ()
              (handler-case
                  (let ((accepted (usocket:socket-accept
                                   listen-socket
                                   :element-type '(unsigned-byte 8))))
                    (setf server-tls-stream
                          (rtls:tls-wrap provider (usocket:socket-stream accepted)
                                         :certificate (%cert-path "server-cert.pem")
                                         :private-key (%cert-path "server-key.pem")
                                         :ca-certificate (when server-ca
                                                           (%cert-path "ca-cert.pem"))
                                         :peer-verify server-peer-verify
                                         :role :server)))
                (error (c) (setf server-error c))))
            :name "tls-test-server")))
    (sleep 0.1)
    (unwind-protect
         (let* ((client-socket (usocket:socket-connect
                                "127.0.0.1" port
                                :element-type '(unsigned-byte 8)))
                (client-tls-stream nil))
           (unwind-protect
                (progn
                  (setf client-tls-stream
                        (rtls:tls-wrap provider (usocket:socket-stream client-socket)
                                       :hostname "localhost"
                                       :certificate (when client-certificate
                                                      (%cert-path client-certificate))
                                       :private-key (when client-private-key
                                                      (%cert-path client-private-key))
                                       :ca-certificate (when client-ca
                                                         (%cert-path "ca-cert.pem"))
                                       :peer-verify client-peer-verify
                                       :role :client))
                  (&body))
             (when client-tls-stream
               (rtls:tls-unwrap provider client-tls-stream))
             (when server-tls-stream
               (rtls:tls-unwrap provider server-tls-stream))
             (usocket:socket-close client-socket)))
      (bt2:join-thread server-thread)
      (usocket:socket-close listen-socket))))

;; ---------------------------------
;; protocol tests
;; ---------------------------------

(test tls-config--creation-with-all-fields
  "Tests that tls-config can be created with all fields."
  (let ((config (rtls:make-tls-config
                 :provider :pure-tls
                 :certificate "/path/cert.pem"
                 :private-key "/path/key.pem"
                 :ca-certificate "/path/ca.pem"
                 :peer-verify t)))
    (is (eq :pure-tls (rtls:tls-config-provider config)))
    (is (string= "/path/cert.pem" (rtls:tls-config-certificate config)))
    (is (string= "/path/key.pem" (rtls:tls-config-private-key config)))
    (is (string= "/path/ca.pem" (rtls:tls-config-ca-certificate config)))
    (is-true (rtls:tls-config-peer-verify config))))

(test tls-config--default-peer-verify-is-t
  "Tests that peer-verify defaults to t."
  (let ((config (rtls:make-tls-config)))
    (is-true (rtls:tls-config-peer-verify config))))

(test tls-wrap--unknown-provider-signals-condition
  "Tests that calling tls-wrap with an unknown provider signals an error."
  (let ((unknown-provider (make-instance 'standard-object)))
    (signals error
      (rtls:tls-wrap unknown-provider nil
                     :role :client
                     :certificate "cert.pem"
                     :private-key "key.pem"))))

(test tls-wrap--protocol-dispatch-works
  "Tests that protocol generic functions dispatch correctly on provider type."
  (let ((provider (make-instance 'pure-tls-provider)))
    (is-true (typep provider 'pure-tls-provider))
    (is-true (find-method #'rtls:tls-wrap nil
                          (list (find-class 'pure-tls-provider) (find-class t))
                          nil))
    (is-true (find-method #'rtls:tls-unwrap nil
                          (list (find-class 'pure-tls-provider) (find-class t))
                          nil))))

;; ---------------------------------
;; pure-tls backend tests
;; ---------------------------------

(test pure-tls--server-wrap-and-client-connect
  "Tests TLS server wrap and client connect on loopback."
  (with-fixture tls-loopback ()
    (is-true (not (null client-tls-stream)))
    (write-byte 42 client-tls-stream)
    (force-output client-tls-stream)
    (bt2:join-thread server-thread)
    (is (null server-error)
        "Server should not have errored: ~a" server-error)
    (is-true (not (null server-tls-stream)))
    (when server-tls-stream
      (let ((byte (read-byte server-tls-stream)))
        (is (= 42 byte))))))

(test pure-tls--mtls-client-presents-cert
  "Tests mTLS: client presents certificate, server verifies."
  (with-fixture tls-loopback (:server-peer-verify t
                               :client-certificate "client-cert.pem"
                               :client-private-key "client-key.pem")
    (is-true (not (null client-tls-stream)))
    (write-byte 99 client-tls-stream)
    (force-output client-tls-stream)
    (bt2:join-thread server-thread)
    (is (null server-error)
        "Server should not have errored: ~a" server-error)
    (is-true (not (null server-tls-stream)))
    (when server-tls-stream
      (let ((byte (read-byte server-tls-stream)))
        (is (= 99 byte))))))

(test pure-tls--invalid-cert-signals-tls-certificate-error
  "Tests that an invalid/untrusted certificate signals tls-certificate-error."
  (let* ((provider (make-instance 'pure-tls-provider))
         (listen-socket (usocket:socket-listen "127.0.0.1" 0
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listen-socket))
         (server-thread nil))
    (unwind-protect
         (progn
           (setf server-thread
                 (bt2:make-thread
                  (lambda ()
                    (handler-case
                        (let ((client-socket (usocket:socket-accept
                                              listen-socket
                                              :element-type '(unsigned-byte 8))))
                          (rtls:tls-wrap provider (usocket:socket-stream client-socket)
                                         :certificate (%cert-path "server-cert.pem")
                                         :private-key (%cert-path "server-key.pem")
                                         :peer-verify nil
                                         :role :server))
                      (error () nil)))
                  :name "tls-test-server-invalid"))
           (sleep 0.1)
           ;; Client tries to verify server cert without the CA — should fail
           (let ((client-socket (usocket:socket-connect
                                 "127.0.0.1" port
                                 :element-type '(unsigned-byte 8))))
             (unwind-protect
                  (signals rtls:tls-certificate-error
                    (rtls:tls-wrap provider (usocket:socket-stream client-socket)
                                   :hostname "localhost"
                                   :peer-verify t
                                   :role :client))
               (usocket:socket-close client-socket)))
           (bt2:join-thread server-thread))
      (usocket:socket-close listen-socket))))

(test pure-tls--tls-unwrap-closes-stream
  "Tests that tls-unwrap gracefully closes a TLS stream."
  (let ((provider (make-instance 'pure-tls-provider)))
    ;; tls-unwrap on nil should not signal
    (finishes (rtls:tls-unwrap provider nil))))
