(defpackage :sento.remoting.transport-test
  (:use :cl :fiveam)
  (:import-from :sento.remoting.transport-tcp
                #:tcp-transport
                #:tcp-transport-actual-port))

(in-package :sento.remoting.transport-test)

(def-suite transport-tests
  :description "Transport protocol and TCP transport tests."
  :in sento.remoting.tests:remoting-test-suite)

(in-suite transport-tests)

;; ---------------------------------
;; test cert paths
;; ---------------------------------

(defun %certs-dir ()
  (asdf:system-relative-pathname "sento-remoting" "tests/remoting/certs/"))

(defun %cert-path (filename)
  (merge-pathnames filename (%certs-dir)))

;; ---------------------------------
;; helpers
;; ---------------------------------

(defun %make-tls-config ()
  "Create a TLS config for tests using the test certificates."
  (rtls:make-tls-config
   :provider :pure-tls
   :certificate (%cert-path "server-cert.pem")
   :private-key (%cert-path "server-key.pem")
   :ca-certificate (%cert-path "ca-cert.pem")
   :peer-verify nil))

(defun %make-test-envelope (&key (target "/user/foo") (sender "/user/bar")
                                  (message-text "hello") (message-type :tell))
  "Create a test envelope with a serialized message."
  (let ((serializer (make-instance 'rseri:sexp-serializer)))
    (renv:make-envelope
     :target-path target
     :sender-path sender
     :message (rseri:serialize serializer message-text)
     :message-type message-type)))

(defmacro %with-transport-pair ((&key (tls t)) &body body)
  "Create a pair of transports (server + client-side) and execute BODY.
Binds: server-transport, server-port, client-transport."
  (let ((config-form (if tls '(%make-tls-config) nil)))
    `(let* ((received-envelopes nil)
            (received-lock (bt2:make-lock :name "received-lock"))
            (server-transport (make-instance 'tcp-transport
                                             :host "127.0.0.1"
                                             :port 0
                                             :tls-config ,config-form))
            (client-transport (make-instance 'tcp-transport
                                             :host "127.0.0.1"
                                             :port 0
                                             :tls-config ,config-form)))
       (declare (ignorable received-lock))
       (rtrans:transport-start
        server-transport
        (lambda (envelope)
          (bt2:with-lock-held (received-lock)
            (push envelope received-envelopes))))
       (let ((server-port (tcp-transport-actual-port server-transport)))
         (declare (ignorable server-port))
         ;; Start client transport too (it needs to be running for sends)
         (rtrans:transport-start client-transport (lambda (env) (declare (ignore env))))
         (unwind-protect
              (progn ,@body)
           (rtrans:transport-stop client-transport)
           (rtrans:transport-stop server-transport))))))

;; ---------------------------------
;; lifecycle tests
;; ---------------------------------

(test transport--start-stop-lifecycle
  "Tests that transport starts and stops cleanly, binding and unbinding the listener."
  (let ((transport (make-instance 'tcp-transport
                                  :host "127.0.0.1"
                                  :port 0)))
    (is-false (rtrans:transport-running-p transport))
    (rtrans:transport-start transport (lambda (env) (declare (ignore env))))
    (is-true (rtrans:transport-running-p transport))
    (is-true (> (tcp-transport-actual-port transport) 0))
    (rtrans:transport-stop transport)
    (is-false (rtrans:transport-running-p transport))))

(test transport--stop-closes-pending-connections
  "Tests that stopping transport closes all active connections."
  (%with-transport-pair (:tls nil)
    (let ((envelope (%make-test-envelope)))
      ;; Send to establish a connection
      (rtrans:transport-send client-transport "127.0.0.1" server-port envelope)
      (sleep 0.1)
      ;; Stop should close everything without errors
      (rtrans:transport-stop client-transport)
      (rtrans:transport-stop server-transport)
      ;; Transports stopped — verify not running
      (is-false (rtrans:transport-running-p client-transport))
      (is-false (rtrans:transport-running-p server-transport)))))

;; ---------------------------------
;; framing tests
;; ---------------------------------

(test transport--send-envelope-between-transports
  "Tests sending an envelope between two transports on loopback (no TLS)."
  (%with-transport-pair (:tls nil)
    (let ((envelope (%make-test-envelope :message-text "test-message")))
      (rtrans:transport-send client-transport "127.0.0.1" server-port envelope)
      (is-true (miscutils:await-cond 1.0
                 (bt2:with-lock-held (received-lock)
                   (= 1 (length received-envelopes)))))
      (let ((received (first received-envelopes)))
        (is (string= "/user/foo" (renv:envelope-target-path received)))
        (is (string= "/user/bar" (renv:envelope-sender-path received)))
        (is (eq :tell (renv:envelope-message-type received)))))))

(test transport--large-message
  "Tests sending a large message (> 64KB) through the transport."
  (%with-transport-pair (:tls nil)
    (let* ((large-text (make-string 100000 :initial-element #\x))
           (envelope (%make-test-envelope :message-text large-text)))
      (rtrans:transport-send client-transport "127.0.0.1" server-port envelope)
      (is-true (miscutils:await-cond 2.0
                 (bt2:with-lock-held (received-lock)
                   (= 1 (length received-envelopes)))))
      (let* ((received (first received-envelopes))
             (serializer (make-instance 'rseri:sexp-serializer))
             (decoded (rseri:deserialize serializer (renv:envelope-message received))))
        (is (= 100000 (length decoded)))))))

;; ---------------------------------
;; connection pooling tests
;; ---------------------------------

(test transport--connection-reuse
  "Tests that a second send reuses the cached connection."
  (%with-transport-pair (:tls nil)
    (let ((env1 (%make-test-envelope :message-text "first"))
          (env2 (%make-test-envelope :message-text "second")))
      (rtrans:transport-send client-transport "127.0.0.1" server-port env1)
      (rtrans:transport-send client-transport "127.0.0.1" server-port env2)
      (is-true (miscutils:await-cond 1.0
                 (bt2:with-lock-held (received-lock)
                   (= 2 (length received-envelopes))))))))

(test transport--connection-failure-reconnect
  "Tests that after a connection failure, the next send reconnects."
  (let* ((received-envelopes nil)
         (received-lock (bt2:make-lock :name "received-lock"))
         (server1 (make-instance 'tcp-transport :host "127.0.0.1" :port 0))
         (client (make-instance 'tcp-transport :host "127.0.0.1" :port 0)))
    (rtrans:transport-start server1
                            (lambda (env)
                              (bt2:with-lock-held (received-lock)
                                (push env received-envelopes))))
    (let ((port1 (tcp-transport-actual-port server1)))
      (rtrans:transport-start client (lambda (env) (declare (ignore env))))
      (unwind-protect
           (progn
             ;; Send first message
             (rtrans:transport-send client "127.0.0.1" port1
                                    (%make-test-envelope :message-text "msg1"))
             (is-true (miscutils:await-cond 1.0
                        (bt2:with-lock-held (received-lock)
                          (= 1 (length received-envelopes)))))
             ;; Stop server1 — simulates connection failure
             (rtrans:transport-stop server1)
             (sleep 0.2)
             ;; Start a new server on the same port
             (let ((server2 (make-instance 'tcp-transport :host "127.0.0.1" :port port1)))
               (rtrans:transport-start server2
                                       (lambda (env)
                                         (bt2:with-lock-held (received-lock)
                                           (push env received-envelopes))))
               (unwind-protect
                    (progn
                      ;; The first send may fail (broken connection) — that's expected
                      (handler-case
                          (rtrans:transport-send client "127.0.0.1" port1
                                                (%make-test-envelope :message-text "msg2"))
                        (rtrans:send-failed-error () nil))
                      ;; Second send should reconnect
                      (rtrans:transport-send client "127.0.0.1" port1
                                             (%make-test-envelope :message-text "msg3"))
                      (is-true (miscutils:await-cond 1.0
                                 (bt2:with-lock-held (received-lock)
                                   (>= (length received-envelopes) 2)))))
                 (rtrans:transport-stop server2))))
        (rtrans:transport-stop client)))))

;; ---------------------------------
;; concurrent sends
;; ---------------------------------

(test transport--concurrent-sends
  "Tests concurrent sends from multiple threads."
  (%with-transport-pair (:tls nil)
    (let ((threads nil)
          (count 10))
      (dotimes (i count)
        (push (bt2:make-thread
               (lambda ()
                 (rtrans:transport-send client-transport "127.0.0.1" server-port
                                        (%make-test-envelope
                                         :message-text (format nil "msg-~a" i))))
               :name (format nil "sender-~a" i))
              threads))
      (dolist (thread threads)
        (bt2:join-thread thread))
      (is-true (miscutils:await-cond 2.0
                 (bt2:with-lock-held (received-lock)
                   (= count (length received-envelopes))))))))

;; ---------------------------------
;; error condition tests
;; ---------------------------------

(test transport--connection-refused-signals-condition
  "Tests that connecting to a non-listening port signals connection-refused-error."
  (let ((transport (make-instance 'tcp-transport :host "127.0.0.1" :port 0)))
    (rtrans:transport-start transport (lambda (env) (declare (ignore env))))
    (unwind-protect
         (signals rtrans:connection-refused-error
           (rtrans:transport-send transport "127.0.0.1" 1
                                  (%make-test-envelope)))
      (rtrans:transport-stop transport))))

(test transport--tls-handshake-failure-propagated
  "Tests that a TLS handshake failure is propagated as tls-handshake-error."
  (let* ((server-config (rtls:make-tls-config
                         :provider :pure-tls
                         :certificate (%cert-path "server-cert.pem")
                         :private-key (%cert-path "server-key.pem")
                         :ca-certificate (%cert-path "ca-cert.pem")
                         :peer-verify nil))
         ;; Client has no CA cert and verifies — should fail
         (client-config (rtls:make-tls-config
                         :provider :pure-tls
                         :certificate nil
                         :private-key nil
                         :ca-certificate nil
                         :peer-verify t))
         (server (make-instance 'tcp-transport
                                :host "127.0.0.1" :port 0
                                :tls-config server-config))
         (client (make-instance 'tcp-transport
                                :host "127.0.0.1" :port 0
                                :tls-config client-config)))
    (rtrans:transport-start server (lambda (env) (declare (ignore env))))
    (rtrans:transport-start client (lambda (env) (declare (ignore env))))
    (let ((port (tcp-transport-actual-port server)))
      (unwind-protect
           (signals rtls:tls-error
             (rtrans:transport-send client "127.0.0.1" port
                                    (%make-test-envelope)))
        (rtrans:transport-stop client)
        (rtrans:transport-stop server)))))

;; ---------------------------------
;; TLS transport test
;; ---------------------------------

(test transport--send-envelope-over-tls
  "Tests sending an envelope between two transports over TLS on loopback."
  (%with-transport-pair (:tls t)
    (let ((envelope (%make-test-envelope :message-text "tls-hello")))
      (rtrans:transport-send client-transport "127.0.0.1" server-port envelope)
      (is-true (miscutils:await-cond 2.0
                 (bt2:with-lock-held (received-lock)
                   (= 1 (length received-envelopes)))))
      (let ((received (first received-envelopes)))
        (is (string= "/user/foo" (renv:envelope-target-path received)))
        (is (eq :tell (renv:envelope-message-type received)))))))
