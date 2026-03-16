(defpackage :sento.remoting.envelope
  (:use :cl)
  (:nicknames :renv)
  (:export #:envelope
           #:make-envelope
           #:envelope-target-path
           #:envelope-sender-path
           #:envelope-message
           #:envelope-message-type
           #:envelope-correlation-id
           #:envelope-for-reply
           #:error-envelope
           #:make-error-envelope
           #:error-envelope-correlation-id
           #:error-envelope-error-type
           #:error-envelope-error-message))

(in-package :sento.remoting.envelope)

;; ---------------------------------
;; envelope
;; ---------------------------------

(defstruct envelope
  "Network message envelope for remoting."
  (target-path (error "'target-path' is required!")
   :type string)
  (sender-path nil
   :type (or null string))
  (message nil
   :type (or null (simple-array (unsigned-byte 8) (*))))
  (message-type (error "'message-type' is required!")
   :type keyword)
  (correlation-id nil
   :type (or null string)))

;; ---------------------------------
;; envelope-for-reply
;; ---------------------------------

(defun envelope-for-reply (original-envelope response-message)
  "Create a response envelope from ORIGINAL-ENVELOPE, swapping target/sender
and carrying the correlation-id. RESPONSE-MESSAGE is the serialized payload."
  (make-envelope
   :target-path (envelope-sender-path original-envelope)
   :sender-path (envelope-target-path original-envelope)
   :message response-message
   :message-type :tell
   :correlation-id (envelope-correlation-id original-envelope)))

;; ---------------------------------
;; error-envelope
;; ---------------------------------

(defstruct error-envelope
  "Error envelope for remote-side failures."
  (correlation-id (error "'correlation-id' is required!")
   :type string)
  (error-type (error "'error-type' is required!")
   :type keyword)
  (error-message ""
   :type string))
