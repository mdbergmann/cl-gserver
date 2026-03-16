(defpackage :sento.remoting.serialization-test
  (:use :cl :fiveam))

(in-package :sento.remoting.serialization-test)

(def-suite serialization-tests
  :description "Serialization protocol and sexp-serializer tests."
  :in sento.remoting.tests:remoting-test-suite)

(in-suite serialization-tests)

(defvar *serializer* (make-instance 'rseri:sexp-serializer))

(test round-trip--atoms
  "Tests round-trip serialization of numbers, strings, symbols, keywords, characters."
  (dolist (obj (list 42 3.14 -1 "hello world" 'foo :bar #\A))
    (let ((result (rseri:deserialize *serializer*
                                     (rseri:serialize *serializer* obj))))
      (is (equal obj result)
          "Round-trip failed for ~a" obj))))

(test round-trip--nested-lists
  "Tests round-trip serialization of nested list structures."
  (let ((obj '(1 (2 3) ("a" :b (4 5)))))
    (is (equal obj
               (rseri:deserialize *serializer*
                                  (rseri:serialize *serializer* obj))))))

(test round-trip--cons-cells
  "Tests round-trip serialization of cons cells (dotted pairs)."
  (let ((obj '(1 . 2)))
    (is (equal obj
               (rseri:deserialize *serializer*
                                  (rseri:serialize *serializer* obj))))))

(test round-trip--vectors
  "Tests round-trip serialization of vectors."
  (let ((obj #(1 2 3 "four")))
    (is (equalp obj
                (rseri:deserialize *serializer*
                                   (rseri:serialize *serializer* obj))))))

(test round-trip--nil-and-empty-list
  "Tests round-trip serialization of nil and empty list."
  (let ((result (rseri:deserialize *serializer*
                                   (rseri:serialize *serializer* nil))))
    (is (null result)))
  (let ((result (rseri:deserialize *serializer*
                                   (rseri:serialize *serializer* '()))))
    (is (null result))))

(defclass test-serializer () ())

(defmethod rseri:serialize ((s test-serializer) object)
  (flexi-streams:string-to-octets (format nil "custom:~a" object)
                                  :external-format :utf-8))

(defmethod rseri:deserialize ((s test-serializer) bytes)
  (let ((str (flexi-streams:octets-to-string bytes :external-format :utf-8)))
    (subseq str 7)))

(test custom-serializer--dispatches-correctly
  "Tests that a custom serializer class dispatches through the protocol."
  (let* ((s (make-instance 'test-serializer))
         (bytes (rseri:serialize s "hello"))
         (result (rseri:deserialize s bytes)))
    (is (string= "hello" result))))
