(defpackage :sento.remoting.envelope-test
  (:use :cl :fiveam))

(in-package :sento.remoting.envelope-test)

(def-suite envelope-tests
  :description "Envelope and error-envelope tests."
  :in sento.remoting.tests:remoting-test-suite)

(in-suite envelope-tests)

(defun %make-test-message ()
  (make-array 5 :element-type '(unsigned-byte 8)
                :initial-contents '(104 101 108 108 111)))

(test create-envelope--verify-all-fields
  "Tests that envelope creation populates all fields correctly."
  (let ((env (renv:make-envelope
              :target-path "/user/greeter"
              :sender-path "sento://192.168.1.2:4712/user/caller"
              :message (%make-test-message)
              :message-type :tell
              :correlation-id "abc-123")))
    (is (string= "/user/greeter" (renv:envelope-target-path env)))
    (is (string= "sento://192.168.1.2:4712/user/caller" (renv:envelope-sender-path env)))
    (is (= 5 (length (renv:envelope-message env))))
    (is (eq :tell (renv:envelope-message-type env)))
    (is (string= "abc-123" (renv:envelope-correlation-id env)))))

(test envelope-for-reply--swaps-paths-and-preserves-correlation-id
  "Tests that envelope-for-reply creates a response envelope with swapped paths."
  (let* ((original (renv:make-envelope
                    :target-path "/user/greeter"
                    :sender-path "sento://192.168.1.2:4712/user/caller"
                    :message (%make-test-message)
                    :message-type :ask-s
                    :correlation-id "corr-456"))
         (response-msg (make-array 2 :element-type '(unsigned-byte 8)
                                     :initial-contents '(111 107)))
         (reply (renv:envelope-for-reply original response-msg)))
    (is (string= "sento://192.168.1.2:4712/user/caller"
                 (renv:envelope-target-path reply)))
    (is (string= "/user/greeter"
                 (renv:envelope-sender-path reply)))
    (is (= 2 (length (renv:envelope-message reply))))
    (is (string= "corr-456" (renv:envelope-correlation-id reply)))))

(test create-envelope--missing-required-fields-signals-error
  "Tests that creating an envelope without required fields signals an error."
  (signals error
    (eval '(renv:make-envelope :sender-path "foo" :message-type :tell)))
  (signals error
    (eval '(renv:make-envelope :target-path "/user/foo"))))

(test create-envelope--message-type-values
  "Tests that envelope accepts valid message-type keywords."
  (dolist (mt '(:tell :ask-s :ask))
    (let ((env (renv:make-envelope :target-path "/user/foo"
                                   :message-type mt)))
      (is (eq mt (renv:envelope-message-type env))))))

(test create-error-envelope--verify-fields
  "Tests that error-envelope creation populates all fields correctly."
  (let ((ee (renv:make-error-envelope
             :correlation-id "err-789"
             :error-type :actor-not-found
             :error-message "No actor at /user/missing")))
    (is (string= "err-789" (renv:error-envelope-correlation-id ee)))
    (is (eq :actor-not-found (renv:error-envelope-error-type ee)))
    (is (string= "No actor at /user/missing" (renv:error-envelope-error-message ee)))))
