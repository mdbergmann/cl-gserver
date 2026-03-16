(defpackage :sento.remoting.serialization
  (:use :cl)
  (:nicknames :rseri)
  (:export #:serialize
           #:deserialize
           #:sexp-serializer
           ;; conditions
           #:serialization-error
           #:serialization-error-object
           #:deserialization-error
           #:deserialization-error-bytes))

(in-package :sento.remoting.serialization)

;; ---------------------------------
;; conditions
;; ---------------------------------

(define-condition serialization-error (rem:remoting-error)
  ((object :initarg :object
           :reader serialization-error-object
           :documentation "The object that could not be serialized."))
  (:report (lambda (c stream)
             (format stream "Serialization error: cannot serialize ~a: ~a"
                     (serialization-error-object c)
                     (rem:remoting-error-message c))))
  (:documentation "Signaled when an object cannot be serialized."))

(define-condition deserialization-error (rem:remoting-error)
  ((bytes :initarg :bytes
          :reader deserialization-error-bytes
          :documentation "The bytes that could not be deserialized."))
  (:report (lambda (c stream)
             (format stream "Deserialization error: ~a"
                     (rem:remoting-error-message c))))
  (:documentation "Signaled when received bytes cannot be deserialized."))

;; ---------------------------------
;; protocol
;; ---------------------------------

(defgeneric serialize (serializer object)
  (:documentation "Serialize OBJECT using SERIALIZER. Returns a byte vector (unsigned-byte 8)."))

(defgeneric deserialize (serializer bytes)
  (:documentation "Deserialize BYTES using SERIALIZER. Returns the reconstructed object."))

;; ---------------------------------
;; sexp-serializer
;; ---------------------------------

(defclass sexp-serializer ()
  ()
  (:documentation "Default serializer using s-expression representation with UTF-8 encoding.
Supported types: symbols, strings, numbers, lists, cons cells, keywords, characters, vectors, hash-tables (as alists)."))

(defmethod serialize ((serializer sexp-serializer) object)
  (handler-case
      (let ((string (write-to-string object :readably t)))
        (flexi-streams:string-to-octets string :external-format :utf-8))
    (error (c)
      (error 'serialization-error
             :object object
             :message (format nil "~a" c)))))

(defmethod deserialize ((serializer sexp-serializer) bytes)
  (handler-case
      (let ((string (flexi-streams:octets-to-string bytes :external-format :utf-8)))
        (values (read-from-string string)))
    (error (c)
      (error 'deserialization-error
             :bytes bytes
             :message (format nil "~a" c)))))
