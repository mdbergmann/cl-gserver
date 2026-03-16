(defpackage :sento.remoting.conditions-test
  (:use :cl :fiveam))

(in-package :sento.remoting.conditions-test)

(def-suite conditions-tests
  :description "Remoting condition hierarchy tests."
  :in sento.remoting.tests:remoting-test-suite)

(in-suite conditions-tests)

(test create-remoting-error--slots-and-report
  "Tests that remoting-error can be created with message and reports correctly."
  (let ((c (make-condition 'rem:remoting-error :message "something broke")))
    (is (string= "something broke" (rem:remoting-error-message c)))
    (is (search "something broke" (format nil "~a" c)))))

(test create-serialization-conditions--slots-and-report
  "Tests serialization-error and deserialization-error creation and reporting."
  (let ((se (make-condition 'rseri:serialization-error
                            :object '(1 2 3)
                            :message "unsupported type"))
        (de (make-condition 'rseri:deserialization-error
                            :bytes (make-array 3 :element-type '(unsigned-byte 8)
                                                 :initial-contents '(1 2 3))
                            :message "corrupt data")))
    (is (equal '(1 2 3) (rseri:serialization-error-object se)))
    (is (search "unsupported type" (format nil "~a" se)))
    (is (= 3 (length (rseri:deserialization-error-bytes de))))
    (is (search "corrupt data" (format nil "~a" de)))))

(test condition-hierarchy--all-subtypes-of-remoting-error
  "Tests that all conditions are subtypes of remoting-error."
  (is-true (subtypep 'rseri:serialization-error 'rem:remoting-error))
  (is-true (subtypep 'rseri:deserialization-error 'rem:remoting-error))
  (is-true (subtypep 'rtls:tls-error 'rem:remoting-error))
  (is-true (subtypep 'rtls:tls-handshake-error 'rtls:tls-error))
  (is-true (subtypep 'rtls:tls-certificate-error 'rtls:tls-error))
  (is-true (subtypep 'rtls:tls-peer-verify-error 'rtls:tls-error)))

(test create-tls-conditions--slots-and-report
  "Tests TLS condition creation and reporting."
  (let ((he (make-condition 'rtls:tls-handshake-error
                            :reason "protocol mismatch"
                            :message "handshake failed"))
        (ce (make-condition 'rtls:tls-certificate-error
                            :reason "expired"
                            :certificate-subject "CN=test"
                            :message "cert error"))
        (pe (make-condition 'rtls:tls-peer-verify-error
                            :message "no cert")))
    (is (string= "protocol mismatch" (rtls:tls-handshake-error-reason he)))
    (is (search "protocol mismatch" (format nil "~a" he)))
    (is (string= "expired" (rtls:tls-certificate-error-reason ce)))
    (is (string= "CN=test" (rtls:tls-certificate-error-subject ce)))
    (is (search "expired" (format nil "~a" ce)))
    (is (search "peer" (format nil "~a" pe)))))
