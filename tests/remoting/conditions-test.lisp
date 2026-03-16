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
  (is-true (subtypep 'rseri:deserialization-error 'rem:remoting-error)))
