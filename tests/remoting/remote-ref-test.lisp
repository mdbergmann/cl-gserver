(defpackage :sento.remoting.remote-ref-test
  (:use :cl :fiveam)
  (:import-from :sento.remoting.remote-ref
                #:remote-actor-ref
                #:make-remote-ref
                #:remote-host
                #:remote-port
                #:target-path
                #:sender-actor
                #:invalid-remote-uri-error
                #:%handle-response
                #:%stop-sender-actor)
  (:import-from :sento.remoting.transport
                #:transport-start
                #:transport-stop
                #:transport-send
                #:transport-running-p
                #:connection-refused-error
                #:send-failed-error)
  (:import-from :sento.remoting.transport-tcp
                #:tcp-transport
                #:tcp-transport-actual-port)
  (:import-from :sento.remoting.envelope
                #:make-envelope
                #:envelope-target-path
                #:envelope-sender-path
                #:envelope-message
                #:envelope-message-type
                #:envelope-correlation-id
                #:envelope-for-reply)
  (:import-from :sento.remoting.serialization
                #:sexp-serializer
                #:serialize
                #:deserialize)
  (:import-from :bordeaux-threads-2
                #:make-lock
                #:with-lock-held
                #:make-thread
                #:join-thread)
  (:import-from :miscutils
                #:await-cond))

(in-package :sento.remoting.remote-ref-test)

(def-suite remote-ref-tests
  :description "Remote actor reference tests."
  :in sento.remoting.tests:remoting-test-suite)

(in-suite remote-ref-tests)

;; ---------------------------------
;; fixtures
;; ---------------------------------

(def-fixture test-system ()
  (let ((system (asys:make-actor-system
                 '(:dispatchers (:shared (:workers 2))))))
    (unwind-protect
         (&body)
      (ac:shutdown system))))

(def-fixture transport-pair ()
  (let* ((received-envelopes nil)
         (received-lock (make-lock :name "received-lock"))
         (server-transport (make-instance 'tcp-transport
                                          :host "127.0.0.1"
                                          :port 0))
         (client-transport (make-instance 'tcp-transport
                                          :host "127.0.0.1"
                                          :port 0)))
    (transport-start
     server-transport
     (lambda (envelope)
       (with-lock-held (received-lock)
         (push envelope received-envelopes))))
    (let ((server-port (tcp-transport-actual-port server-transport)))
      (transport-start client-transport (lambda (env) (declare (ignore env))))
      (unwind-protect
           (&body)
        (transport-stop client-transport)
        (transport-stop server-transport)))))

(def-fixture echo-transport ()
  (let* ((serializer (make-instance 'sexp-serializer))
         (server-transport (make-instance 'tcp-transport
                                          :host "127.0.0.1"
                                          :port 0))
         (client-transport (make-instance 'tcp-transport
                                          :host "127.0.0.1"
                                          :port 0)))
    (transport-start
     server-transport
     (lambda (envelope)
       ;; Echo back: deserialize message, prefix with "echo:", send reply
       (let* ((msg (deserialize serializer (envelope-message envelope)))
              (reply-msg (format nil "echo:~a" msg))
              (reply-envelope (envelope-for-reply
                               envelope
                               (serialize serializer reply-msg))))
         (handler-case
             (transport-send server-transport
                             "127.0.0.1"
                             (tcp-transport-actual-port client-transport)
                             reply-envelope)
           (error (c)
             (log:warn "Echo server send error: ~a" c))))))
    (let ((server-port (tcp-transport-actual-port server-transport)))
      (transport-start client-transport
                       (lambda (envelope)
                         (declare (ignore envelope))))
      (unwind-protect
           (&body)
        (transport-stop client-transport)
        (transport-stop server-transport)))))

;; ---------------------------------
;; URI parsing tests
;; ---------------------------------

(test remote-ref--parse-valid-uri
  "Tests that a valid sento:// URI is parsed correctly."
  (with-fixture test-system ()
    (let* ((serializer (make-instance 'sexp-serializer))
           (transport (make-instance 'tcp-transport :host "127.0.0.1" :port 0))
           (ref (make-remote-ref system
                                 "sento://192.168.1.1:4711/user/greeter"
                                 transport
                                 serializer)))
      (is (string= "192.168.1.1" (remote-host ref)))
      (is (= 4711 (remote-port ref)))
      (is (string= "/user/greeter" (target-path ref))))))

(test remote-ref--invalid-uri-signals-condition
  "Tests that malformed URIs signal invalid-remote-uri-error."
  (with-fixture test-system ()
    (let ((serializer (make-instance 'sexp-serializer))
          (transport (make-instance 'tcp-transport :host "127.0.0.1" :port 0)))
      ;; Missing scheme
      (signals invalid-remote-uri-error
        (make-remote-ref system "http://host:1234/path" transport serializer))
      ;; Missing port
      (signals invalid-remote-uri-error
        (make-remote-ref system "sento://host/path" transport serializer))
      ;; Missing path
      (signals invalid-remote-uri-error
        (make-remote-ref system "sento://host:1234" transport serializer))
      ;; Empty host
      (signals invalid-remote-uri-error
        (make-remote-ref system "sento://:1234/path" transport serializer))
      ;; Not a string
      (signals invalid-remote-uri-error
        (make-remote-ref system 42 transport serializer)))))

;; ---------------------------------
;; tell tests
;; ---------------------------------

(test remote-ref--tell-enqueues-and-returns-immediately
  "Tests that tell enqueues to sender actor and returns immediately (non-blocking)."
  (with-fixture test-system ()
    (with-fixture transport-pair ()
      (let* ((serializer (make-instance 'sexp-serializer))
             (ref (make-remote-ref system
                                   (format nil "sento://127.0.0.1:~a/user/target" server-port)
                                   client-transport
                                   serializer)))
        ;; tell should return immediately
        (act:tell ref "hello")
        ;; The message should arrive at the server
        (is-true (await-cond 1.0
                   (with-lock-held (received-lock)
                     (= 1 (length received-envelopes)))))
        (let ((received (with-lock-held (received-lock)
                          (first received-envelopes))))
          (is (string= "/user/target" (envelope-target-path received)))
          (is (eq :tell (envelope-message-type received)))
          (is (null (envelope-correlation-id received)))
          ;; Deserialize and check the message
          (let ((msg (deserialize serializer (envelope-message received))))
            (is (string= "hello" msg))))))))

(test remote-ref--tell-envelope-has-correct-message-type
  "Tests that tell creates envelope with :tell message-type."
  (with-fixture test-system ()
    (with-fixture transport-pair ()
      (let* ((serializer (make-instance 'sexp-serializer))
             (ref (make-remote-ref system
                                   (format nil "sento://127.0.0.1:~a/user/foo" server-port)
                                   client-transport
                                   serializer)))
        (act:tell ref '(:test-data 42))
        (is-true (await-cond 1.0
                   (with-lock-held (received-lock)
                     (= 1 (length received-envelopes)))))
        (let ((received (with-lock-held (received-lock)
                          (first received-envelopes))))
          (is (eq :tell (envelope-message-type received)))
          (let ((msg (deserialize serializer (envelope-message received))))
            (is (equal '(:test-data 42) msg))))))))

(test remote-ref--tell-transport-error-is-logged-not-signaled
  "Tests that transport errors on tell are logged but not signaled to the caller."
  (with-fixture test-system ()
    (let* ((serializer (make-instance 'sexp-serializer))
           (client-transport (make-instance 'tcp-transport
                                            :host "127.0.0.1"
                                            :port 0)))
      (transport-start client-transport (lambda (env) (declare (ignore env))))
      (unwind-protect
           (let ((ref (make-remote-ref system
                                       "sento://127.0.0.1:1/user/unreachable"
                                       client-transport
                                       serializer)))
             (declare (ignore ref))
             ;; tell to a non-existent port should NOT signal — just log
             (finishes (act:tell ref "message"))
             ;; Give the sender actor time to process
             (sleep 0.5))
        (transport-stop client-transport)))))

;; ---------------------------------
;; ask-s tests
;; ---------------------------------

(test remote-ref--ask-s-sends-directly-and-returns-response
  "Tests that ask-s sends directly and returns the deserialized response."
  (with-fixture test-system ()
    (with-fixture echo-transport ()
      (let ((ref (make-remote-ref system
                                  (format nil "sento://127.0.0.1:~a/user/echo" server-port)
                                  client-transport
                                  serializer)))
        ;; Wire the client's inbound handler to route responses to the ref
        (setf (rtrans::%transport-message-handler client-transport)
              (lambda (envelope)
                (%handle-response ref envelope)))
        (let ((result (act:ask-s ref "world" :time-out 2)))
          (is (string= "echo:world" result)))))))

(test remote-ref--ask-s-timeout-returns-handler-error
  "Tests that ask-s timeout returns (cons :handler-error ask-timeout)."
  (with-fixture test-system ()
    (with-fixture transport-pair ()
      ;; Server receives but never responds — so ask-s will time out
      (let* ((serializer (make-instance 'sexp-serializer))
             (ref (make-remote-ref system
                                   (format nil "sento://127.0.0.1:~a/user/silent" server-port)
                                   client-transport
                                   serializer)))
        (let ((result (act:ask-s ref "hello" :time-out 0.5)))
          (is (consp result))
          (is (eq :handler-error (car result)))
          (is (typep (cdr result) 'timeutils:ask-timeout)))))))

(test remote-ref--ask-s-transport-error-signals-immediately
  "Tests that ask-s transport errors signal immediately to the caller."
  (with-fixture test-system ()
    (let* ((serializer (make-instance 'sexp-serializer))
           (client-transport (make-instance 'tcp-transport
                                            :host "127.0.0.1"
                                            :port 0)))
      (transport-start client-transport (lambda (env) (declare (ignore env))))
      (unwind-protect
           (let ((ref (make-remote-ref system
                                       "sento://127.0.0.1:1/user/unreachable"
                                       client-transport
                                       serializer)))
             (declare (ignore ref))
             (signals connection-refused-error
               (act:ask-s ref "hello" :time-out 2)))
        (transport-stop client-transport)))))

;; ---------------------------------
;; ask tests
;; ---------------------------------

(test remote-ref--ask-returns-future-resolved-via-correlation-id
  "Tests that ask returns a future that resolves via correlation-id response."
  (with-fixture test-system ()
    (with-fixture echo-transport ()
      (let ((ref (make-remote-ref system
                                  (format nil "sento://127.0.0.1:~a/user/echo" server-port)
                                  client-transport
                                  serializer)))
        (setf (rtrans::%transport-message-handler client-transport)
              (lambda (envelope)
                (%handle-response ref envelope)))
        (let ((fut (act:ask ref "test")))
          (is (typep fut 'future:future))
          (is-true (await-cond 2.0 (future:complete-p fut)))
          (is (string= "echo:test" (future:fresult fut))))))))

(test remote-ref--ask-timeout-resolves-future-with-error
  "Tests that ask timeout resolves the future with a handler-error."
  (with-fixture test-system ()
    (with-fixture transport-pair ()
      ;; Server receives but never responds
      (let* ((serializer (make-instance 'sexp-serializer))
             (ref (make-remote-ref system
                                   (format nil "sento://127.0.0.1:~a/user/silent" server-port)
                                   client-transport
                                   serializer)))
        (let ((fut (act:ask ref "hello" :time-out 0.5)))
          (is (typep fut 'future:future))
          (is-true (await-cond 2.0 (future:complete-p fut)))
          (let ((result (future:fresult fut)))
            (is (consp result))
            (is (eq :handler-error (car result)))
            (is (typep (cdr result) 'timeutils:ask-timeout))))))))

;; ---------------------------------
;; path test
;; ---------------------------------

(test remote-ref--path-returns-full-uri
  "Tests that act:path on a remote-ref returns the full sento:// URI."
  (with-fixture test-system ()
    (let* ((serializer (make-instance 'sexp-serializer))
           (transport (make-instance 'tcp-transport :host "127.0.0.1" :port 0))
           (ref (make-remote-ref system
                                 "sento://myhost:4711/user/greeter"
                                 transport
                                 serializer)))
      (is (string= "sento://myhost:4711/user/greeter" (act:path ref))))))
