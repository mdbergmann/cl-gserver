(defpackage :sento.remoting.remoting-test
  (:use :cl :fiveam)
  (:import-from :sento.remoting
                #:enable-remoting
                #:disable-remoting
                #:remoting-enabled-p
                #:remoting-port
                #:make-remote-ref
                #:remoting-error)
  (:import-from :sento.remoting.remote-ref
                #:remote-actor-ref
                #:remote-host
                #:remote-port
                #:target-path
                #:stop-sender-actor)
  (:import-from :miscutils
                #:await-cond))

(in-package :sento.remoting.remoting-test)

(def-suite remoting-integration-tests
  :description "End-to-end remoting integration tests."
  :in sento.remoting.tests:remoting-test-suite)

(in-suite remoting-integration-tests)

;; ---------------------------------
;; fixtures
;; ---------------------------------

(def-fixture single-system ()
  "A single actor-system with remoting enabled."
  (let ((system (asys:make-actor-system
                  '(:dispatchers (:shared (:workers 2))))))
    (unwind-protect
         (&body)
      (disable-remoting system)
      (ac:shutdown system))))

(def-fixture two-systems ()
  "Two actor-systems with remoting enabled, ready for cross-system communication."
  (let ((system-a (asys:make-actor-system
                    '(:dispatchers (:shared (:workers 2)))))
        (system-b (asys:make-actor-system
                    '(:dispatchers (:shared (:workers 2))))))
    (enable-remoting system-a :host "127.0.0.1" :port 0)
    (enable-remoting system-b :host "127.0.0.1" :port 0)
    (unwind-protect
         (&body)
      (disable-remoting system-a)
      (disable-remoting system-b)
      (ac:shutdown system-a)
      (ac:shutdown system-b))))

;; ---------------------------------
;; enable/disable tests
;; ---------------------------------

(test enable-remoting--starts-transport
  "Tests that enable-remoting starts the transport and makes remoting available."
  (with-fixture single-system ()
    (is-false (remoting-enabled-p system))
    (enable-remoting system :host "127.0.0.1" :port 0)
    (is-true (remoting-enabled-p system))
    (is (numberp (remoting-port system)))
    (is (plusp (remoting-port system)))))

(test enable-remoting--error-when-already-enabled
  "Tests that enabling remoting twice signals an error."
  (with-fixture single-system ()
    (enable-remoting system :host "127.0.0.1" :port 0)
    (signals remoting-error
      (enable-remoting system :host "127.0.0.1" :port 0))))

(test disable-remoting--stops-transport
  "Tests that disable-remoting stops the transport and cleans up."
  (with-fixture single-system ()
    (enable-remoting system :host "127.0.0.1" :port 0)
    (is-true (remoting-enabled-p system))
    (disable-remoting system)
    (is-false (remoting-enabled-p system))
    (is (null (remoting-port system)))))

(test disable-remoting--noop-when-not-enabled
  "Tests that disable-remoting is a no-op when remoting is not enabled."
  (with-fixture single-system ()
    (is (null (disable-remoting system)))))

;; ---------------------------------
;; make-remote-ref tests
;; ---------------------------------

(test make-remote-ref--creates-ref-via-context
  "Tests that make-remote-ref creates a properly configured remote-ref."
  (with-fixture single-system ()
    (enable-remoting system :host "127.0.0.1" :port 0)
    (let ((ref (make-remote-ref system "sento://192.168.1.1:4711/user/greeter")))
      (is (typep ref 'remote-actor-ref))
      (is (string= "192.168.1.1" (remote-host ref)))
      (is (= 4711 (remote-port ref)))
      (is (string= "/user/greeter" (target-path ref))))))

(test make-remote-ref--error-when-remoting-not-enabled
  "Tests that make-remote-ref signals an error when remoting is not enabled."
  (with-fixture single-system ()
    (signals remoting-error
      (make-remote-ref system "sento://host:1234/user/actor"))))

;; ---------------------------------
;; tell integration tests
;; ---------------------------------

(test tell--remote-to-local
  "Tests that a remote tell delivers a message to a local actor on another system."
  (with-fixture two-systems ()
(let* ((received nil)
           (received-lock (bt2:make-lock :name "received-lock"))
           (actor-b (ac:actor-of system-b
                                 :name "echo"
                                 :receive (lambda (msg)
                                            (bt2:with-lock-held (received-lock)
                                              (setf received msg))))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/echo" (remoting-port system-b)))))
        (act:tell ref "hello-remote")
        (is-true (await-cond 2.0
                   (bt2:with-lock-held (received-lock)
                     (not (null received)))))
        (bt2:with-lock-held (received-lock)
          (is (string= "hello-remote" received)))))))

(test tell--complex-message
  "Tests that complex messages (lists, nested structures) are delivered correctly."
  (with-fixture two-systems ()
(let* ((received nil)
           (received-lock (bt2:make-lock :name "received-lock"))
           (actor-b (ac:actor-of system-b
                                 :name "collector"
                                 :receive (lambda (msg)
                                            (bt2:with-lock-held (received-lock)
                                              (setf received msg))))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/collector" (remoting-port system-b)))))
        (act:tell ref '(:action :greet :data ("Alice" 42)))
        (is-true (await-cond 2.0
                   (bt2:with-lock-held (received-lock)
                     (not (null received)))))
        (bt2:with-lock-held (received-lock)
          (is (equal '(:action :greet :data ("Alice" 42)) received)))))))

(test tell--multiple-messages
  "Tests that multiple tell messages are delivered in order."
  (with-fixture two-systems ()
(let* ((received nil)
           (received-lock (bt2:make-lock :name "received-lock"))
           (actor-b (ac:actor-of system-b
                                 :name "counter"
                                 :receive (lambda (msg)
                                            (bt2:with-lock-held (received-lock)
                                              (push msg received))))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/counter" (remoting-port system-b)))))
        (dotimes (i 5)
          (act:tell ref i))
        (is-true (await-cond 2.0
                   (bt2:with-lock-held (received-lock)
                     (= 5 (length received)))))
        (bt2:with-lock-held (received-lock)
          (is (equal '(0 1 2 3 4) (reverse received))))))))

(test tell--to-nonexistent-actor--no-error
  "Tests that tell to a non-existent remote actor does not signal on the sender side."
  (with-fixture two-systems ()
(let ((ref (make-remote-ref system-a
                                (format nil "sento://127.0.0.1:~a/user/nonexistent" (remoting-port system-b)))))
      ;; Should not signal — the remote side logs a warning
      (finishes (act:tell ref "hello"))
      ;; Give time for delivery
      (sleep 0.5))))

;; ---------------------------------
;; ask-s integration tests
;; ---------------------------------

(test ask-s--remote-to-local
  "Tests that a remote ask-s sends a message and receives the response."
  (with-fixture two-systems ()
(let ((actor-b (ac:actor-of system-b
                                :name "greeter"
                                :receive (lambda (msg)
                                           (format nil "Hello, ~a!" msg)))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/greeter" (remoting-port system-b)))))
        (let ((result (act:ask-s ref "World" :time-out 3)))
          (is (string= "Hello, World!" result)))))))

(test ask-s--returns-complex-result
  "Tests that ask-s correctly returns complex data structures."
  (with-fixture two-systems ()
(let ((actor-b (ac:actor-of system-b
                                :name "compute"
                                :receive (lambda (msg)
                                           (list :result (* msg msg))))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/compute" (remoting-port system-b)))))
        (let ((result (act:ask-s ref 7 :time-out 3)))
          (is (equal '(:result 49) result)))))))

(test ask-s--timeout-when-actor-slow
  "Tests that ask-s times out when the remote actor takes too long."
  (with-fixture two-systems ()
(let ((actor-b (ac:actor-of system-b
                                :name "slow"
                                :receive (lambda (msg)
                                           (declare (ignore msg))
                                           (sleep 5)
                                           :done))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/slow" (remoting-port system-b)))))
        (let ((result (act:ask-s ref "go" :time-out 0.5)))
          (is (consp result))
          (is (eq :handler-error (car result))))))))

;; ---------------------------------
;; ask integration tests
;; ---------------------------------

(test ask--remote-to-local
  "Tests that a remote ask returns a future that resolves with the response."
  (with-fixture two-systems ()
(let ((actor-b (ac:actor-of system-b
                                :name "adder"
                                :receive (lambda (msg)
                                           (+ (first msg) (second msg))))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/adder" (remoting-port system-b)))))
        (let ((fut (act:ask ref '(3 4) :time-out 3)))
          (is (typep fut 'future:future))
          (is-true (await-cond 3.0 (future:complete-p fut)))
          (is (= 7 (future:fresult fut))))))))

(test ask--timeout-resolves-future-with-error
  "Tests that ask timeout resolves the future with a handler-error."
  (with-fixture two-systems ()
(let ((actor-b (ac:actor-of system-b
                                :name "slow-ask"
                                :receive (lambda (msg)
                                           (declare (ignore msg))
                                           (sleep 5)
                                           :done))))
      (declare (ignore actor-b))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/slow-ask" (remoting-port system-b)))))
        (let ((fut (act:ask ref "go" :time-out 0.5)))
          (is-true (await-cond 3.0 (future:complete-p fut)))
          (let ((result (future:fresult fut)))
            (is (consp result))
            (is (eq :handler-error (car result)))))))))

;; ---------------------------------
;; bidirectional tests
;; ---------------------------------

(test bidirectional--both-systems-communicate
  "Tests that both systems can send messages to each other."
  (with-fixture two-systems ()
    ;; Actor on system A
    (let ((actor-a (ac:actor-of system-a
                                :name "service-a"
                                :receive (lambda (msg)
                                           (format nil "A:~a" msg)))))
      (declare (ignore actor-a))
      ;; Actor on system B
      (let ((actor-b (ac:actor-of system-b
                                  :name "service-b"
                                  :receive (lambda (msg)
                                             (format nil "B:~a" msg)))))
        (declare (ignore actor-b))
        ;; A asks B
        (let ((ref-b (make-remote-ref system-a
                                      (format nil "sento://127.0.0.1:~a/user/service-b" (remoting-port system-b)))))
          (let ((result-b (act:ask-s ref-b "hello" :time-out 3)))
            (is (string= "B:hello" result-b))))
        ;; B asks A
        (let ((ref-a (make-remote-ref system-b
                                      (format nil "sento://127.0.0.1:~a/user/service-a" (remoting-port system-a)))))
          (let ((result-a (act:ask-s ref-a "world" :time-out 3)))
            (is (string= "A:world" result-a))))))))

;; ---------------------------------
;; nested actor path tests
;; ---------------------------------

(test tell--to-nested-actor
  "Tests that tell can reach a nested actor via path."
  (with-fixture two-systems ()
(let* ((received nil)
           (received-lock (bt2:make-lock :name "received-lock"))
           (parent (ac:actor-of system-b
                                :name "parent"
                                :receive (lambda (msg)
                                           (declare (ignore msg))
                                           nil))))
      (ac:actor-of parent
                   :name "child"
                   :receive (lambda (msg)
                              (bt2:with-lock-held (received-lock)
                                (setf received msg))))
      (let ((ref (make-remote-ref system-a
                                  (format nil "sento://127.0.0.1:~a/user/parent/child" (remoting-port system-b)))))
        (act:tell ref "nested-hello")
        (is-true (await-cond 2.0
                   (bt2:with-lock-held (received-lock)
                     (not (null received)))))
        (bt2:with-lock-held (received-lock)
          (is (string= "nested-hello" received)))))))
