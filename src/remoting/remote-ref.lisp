(defpackage :sento.remoting.remote-ref
  (:use :cl)
  (:nicknames :rref)
  (:import-from :sento.remoting
                #:remoting-error
                #:remoting-error-message)
  (:import-from :sento.remoting.envelope
                #:make-envelope
                #:envelope-message
                #:envelope-correlation-id)
  (:import-from :sento.remoting.serialization
                #:serialize
                #:deserialize)
  (:import-from :sento.remoting.transport
                #:transport-send
                #:transport-error)
  (:import-from :bordeaux-threads-2
                #:make-lock
                #:with-lock-held
                #:make-condition-variable
                #:condition-wait
                #:condition-notify
                #:make-thread)
  (:export #:remote-actor-ref
           #:make-remote-ref
           #:remote-host
           #:remote-port
           #:target-path
           #:sender-actor
           ;; conditions
           #:remote-actor-error
           #:invalid-remote-uri-error
           ;; response handling
           #:%handle-response
           #:%stop-sender-actor))

(in-package :sento.remoting.remote-ref)

;; ---------------------------------
;; conditions
;; ---------------------------------

(define-condition remote-actor-error (remoting-error)
  ()
  (:documentation "Base condition for remote actor reference errors."))

(define-condition invalid-remote-uri-error (remote-actor-error)
  ((uri :initarg :uri
        :reader invalid-remote-uri-error-uri
        :documentation "The malformed URI."))
  (:report (lambda (c stream)
             (format stream "Invalid remote URI: ~a (~a)"
                     (invalid-remote-uri-error-uri c)
                     (remoting-error-message c))))
  (:documentation "Signaled when a sento:// URI cannot be parsed."))

;; ---------------------------------
;; URI parsing
;; ---------------------------------

(defun %parse-remote-uri (uri)
  "Parse a sento://host:port/path URI.
Returns (values host port path) or signals invalid-remote-uri-error."
  (unless (and (stringp uri)
               (>= (length uri) 9)
               (string= "sento://" uri :end2 8))
    (error 'invalid-remote-uri-error
           :uri uri
           :message "URI must start with sento://"))
  (let* ((rest (subseq uri 8))
         (colon-pos (position #\: rest))
         (first-slash-pos (position #\/ rest :start (or colon-pos 0))))
    (unless (and colon-pos first-slash-pos (< colon-pos first-slash-pos))
      (error 'invalid-remote-uri-error
             :uri uri
             :message "URI must be in format sento://host:port/path"))
    (let ((host (subseq rest 0 colon-pos))
          (port-str (subseq rest (1+ colon-pos) first-slash-pos))
          (path (subseq rest first-slash-pos)))
      (when (string= host "")
        (error 'invalid-remote-uri-error
               :uri uri
               :message "Host must not be empty"))
      (let ((port (handler-case (parse-integer port-str)
                    (error ()
                      (error 'invalid-remote-uri-error
                             :uri uri
                             :message (format nil "Invalid port: ~a" port-str))))))
        (when (or (<= port 0) (> port 65535))
          (error 'invalid-remote-uri-error
                 :uri uri
                 :message (format nil "Port out of range: ~a" port)))
        (when (string= path "")
          (error 'invalid-remote-uri-error
                 :uri uri
                 :message "Path must not be empty"))
        (values host port path)))))

;; ---------------------------------
;; correlation ID generation
;; ---------------------------------

(defvar %corr-id-counter 0)
(defvar %corr-id-lock (make-lock :name "corr-id-lock"))

(defun %make-correlation-id ()
  "Generate a unique correlation ID string."
  (with-lock-held (%corr-id-lock)
    (format nil "corr-~a-~a" (incf %corr-id-counter) (get-internal-real-time))))

;; ---------------------------------
;; remote-actor-ref
;; ---------------------------------

(defclass remote-actor-ref ()
  ((remote-host :initarg :remote-host
                :reader remote-host
                :documentation "The remote host address.")
   (remote-port :initarg :remote-port
                :reader remote-port
                :documentation "The remote port number.")
   (target-path :initarg :target-path
                :reader target-path
                :documentation "The actor path on the remote system.")
   (transport :initarg :transport
              :reader transport
              :documentation "The transport used for sending messages.")
   (serializer :initarg :serializer
               :reader serializer
               :documentation "The serializer for message encoding/decoding.")
   (sender-actor :reader sender-actor
                 :documentation "Internal actor that queues and sends tell messages via dispatcher.")
   (pending-asks :initform (make-hash-table :test 'equal)
                 :reader pending-asks
                 :documentation "Hash-table: correlation-id -> condvar (ask-s) or future (ask).")
   (pending-asks-lock :initform (make-lock :name "pending-asks-lock")
                      :reader %pending-asks-lock
                      :documentation "Lock for thread-safe access to pending-asks.")
   (system :initarg :system
           :reader system
           :documentation "The actor-system this remote ref belongs to."))
  (:documentation "Proxy for a remote actor. Implements tell/ask-s/ask but sends over the network."))

(defmethod print-object ((obj remote-actor-ref) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a:~a~a"
            (remote-host obj)
            (remote-port obj)
            (target-path obj))))

;; ---------------------------------
;; sender actor setup
;; ---------------------------------

(defun %make-sender-actor (ref system &key dispatcher max-queue-size)
  "Create the internal sender actor for queued tell messages.
Uses the actor-system's dispatcher infrastructure for scalability."
  (let ((receive-fn (lambda (envelope)
                      (handler-case
                          (transport-send (transport ref)
                                          (remote-host ref)
                                          (remote-port ref)
                                          envelope)
                        (transport-error (c)
                          (log:warn "Remote tell send failed for ~a: ~a"
                                    (renv:envelope-target-path envelope) c))))))
    (ac:actor-of system
                 :receive receive-fn
                 :name (string (gensym "remote-sender-"))
                 :dispatcher (or dispatcher :shared)
                 :queue-size (or max-queue-size 0))))

(defmethod initialize-instance :after ((ref remote-actor-ref) &key dispatcher max-queue-size)
  (setf (slot-value ref 'sender-actor)
        (%make-sender-actor ref (system ref)
                            :dispatcher dispatcher
                            :max-queue-size max-queue-size)))

;; ---------------------------------
;; factory
;; ---------------------------------

(defun make-remote-ref (system uri transport serializer &key max-queue-size dispatcher)
  "Create a remote-actor-ref from a sento:// URI.
SYSTEM is the local actor-system (required).
TRANSPORT is the transport to use for sending.
SERIALIZER is the serializer for message encoding.
MAX-QUEUE-SIZE limits the sender actor's queue (nil or 0 = unbounded).
DISPATCHER is the dispatcher identifier for the sender actor (default :shared)."
  (multiple-value-bind (host port path) (%parse-remote-uri uri)
    (make-instance 'remote-actor-ref
                   :remote-host host
                   :remote-port port
                   :target-path path
                   :transport transport
                   :serializer serializer
                   :system system
                   :max-queue-size max-queue-size
                   :dispatcher dispatcher)))

;; ---------------------------------
;; helper: derive sender path
;; ---------------------------------

(defun %local-sender-path (ref)
  "Construct a local sender-path for ask-s/ask response routing."
  (declare (ignore ref))
  "/__local__")

(defun %derive-sender-path (sender)
  "Derive the sender-path for an envelope.
Returns the sender's path, or nil if sender is nil."
  (when sender (act:path sender)))

;; ---------------------------------
;; tell — queued via internal sender actor
;; ---------------------------------

(defmethod act:tell ((ref remote-actor-ref) message &optional sender)
  (let ((envelope (make-envelope
                   :target-path (target-path ref)
                   :sender-path (%derive-sender-path sender)
                   :message (serialize (serializer ref) message)
                   :message-type :tell)))
    (act:tell (sender-actor ref) envelope)))

;; ---------------------------------
;; ask-s — direct send, blocks on condvar
;; ---------------------------------

(defmethod act:ask-s ((ref remote-actor-ref) message &key time-out)
  (let* ((corr-id (%make-correlation-id))
         (envelope (make-envelope
                    :target-path (target-path ref)
                    :sender-path (%local-sender-path ref)
                    :message (serialize (serializer ref) message)
                    :message-type :ask-s
                    :correlation-id corr-id))
         (lock (make-lock :name "ask-s-lock"))
         (cvar (make-condition-variable :name "ask-s-cvar")))
    ;; Register pending ask before sending
    (with-lock-held ((%pending-asks-lock ref))
      (setf (gethash corr-id (pending-asks ref))
            (list :ask-s lock cvar :no-result)))
    ;; Send on caller's thread — transport errors propagate directly
    (handler-case
        (transport-send (transport ref) (remote-host ref) (remote-port ref) envelope)
      (error (c)
        ;; Clean up pending entry on send failure
        (with-lock-held ((%pending-asks-lock ref))
          (remhash corr-id (pending-asks ref)))
        (error c)))
    ;; Block until response or timeout
    (bt2:with-lock-held (lock)
      (bt2:condition-wait cvar lock :timeout time-out))
    ;; Retrieve and clean up
    (let ((entry (with-lock-held ((%pending-asks-lock ref))
                   (prog1 (gethash corr-id (pending-asks ref))
                     (remhash corr-id (pending-asks ref))))))
      (let ((result (fourth entry)))
        (if (eq result :no-result)
            (cons :handler-error
                  (make-condition 'timeutils:ask-timeout
                                  :wait-time time-out))
            result)))))

;; ---------------------------------
;; ask — direct send, returns future
;; ---------------------------------

(defmethod act:ask ((ref remote-actor-ref) message &key time-out)
  (let* ((corr-id (%make-correlation-id))
         (envelope (make-envelope
                    :target-path (target-path ref)
                    :sender-path (%local-sender-path ref)
                    :message (serialize (serializer ref) message)
                    :message-type :ask
                    :correlation-id corr-id)))
    (future:make-future
     (lambda (resolve-fn)
       ;; Register future for correlation-id
       (with-lock-held ((%pending-asks-lock ref))
         (setf (gethash corr-id (pending-asks ref))
               (list :ask resolve-fn)))
       ;; Send on caller's thread — transport errors propagate directly
       (transport-send (transport ref) (remote-host ref) (remote-port ref) envelope)
       ;; Schedule timeout if specified
       (when time-out
         (%schedule-ask-timeout ref corr-id resolve-fn time-out))))))

;; ---------------------------------
;; ask timeout scheduling
;; ---------------------------------

(defun %schedule-ask-timeout (ref corr-id resolve-fn time-out)
  "Schedule a timeout for an ask operation.
After TIME-OUT seconds, if the correlation-id is still pending, resolve with error."
  (make-thread
   (lambda ()
     (sleep time-out)
     (let ((entry (with-lock-held ((%pending-asks-lock ref))
                    (prog1 (gethash corr-id (pending-asks ref))
                      (remhash corr-id (pending-asks ref))))))
       (when entry
         (funcall resolve-fn
                  (cons :handler-error
                        (make-condition 'timeutils:ask-timeout
                                        :wait-time time-out))))))
   :name "remote-ask-timeout"))

;; ---------------------------------
;; response handling (inbound)
;; ---------------------------------

(defun %handle-response (ref envelope)
  "Called by inbound handler when a response envelope arrives.
Match by correlation-id and resolve pending ask."
  (let* ((corr-id (envelope-correlation-id envelope))
         (entry (when corr-id
                  (with-lock-held ((%pending-asks-lock ref))
                    (gethash corr-id (pending-asks ref))))))
    (when entry
      (case (first entry)
        (:ask-s
         ;; Signal the waiting condvar
         (let ((lock (second entry))
               (cvar (third entry))
               (result (deserialize (serializer ref) (envelope-message envelope))))
           (setf (fourth entry) result)
           (bt2:with-lock-held (lock)
             (bt2:condition-notify cvar))))
        (:ask
         ;; Resolve the future
         (let ((resolve-fn (second entry))
               (result (deserialize (serializer ref) (envelope-message envelope))))
           (with-lock-held ((%pending-asks-lock ref))
             (remhash corr-id (pending-asks ref)))
           (funcall resolve-fn result)))))))

;; ---------------------------------
;; path
;; ---------------------------------

(defmethod act:path ((ref remote-actor-ref))
  "Returns the full sento:// URI of this remote actor."
  (format nil "sento://~a:~a~a"
          (remote-host ref)
          (remote-port ref)
          (target-path ref)))

;; ---------------------------------
;; cleanup
;; ---------------------------------

(defun %stop-sender-actor (ref)
  "Stop the internal sender actor."
  (act-cell:stop (sender-actor ref) t))
