(defpackage :cl-gserver.system
  (:use :cl :cl-gserver :cl-gserver.system-api)
  (:nicknames :system)
  (:import-from :dispatcher-api
                #:make-dispatcher)
  (:import-from :dispatcher
                #:dispatcher-bt)
  (:export #:make-system
           #:system))

(in-package :cl-gserver.system)

(defclass system ()
  ((dispatcher :initarg :dispatcher
               :initform nil
               :accessor dispatcher
               :documentation "The message dispatcher."))
  (:documentation
   "A system is a container for GServers/Actors or other incarnations of GServer."))

(defmethod print-object ((obj system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatcher) obj
      (format stream "dispatcher: ~a"
              dispatcher))))

(defun make-system (&key (num-workers 4))
  "Creates a system with 4 wortkers by default."
  (make-instance 'system :dispatcher (make-dispatcher 'dispatcher-bt :num-workers num-workers)))

(defmethod shutdown ((self system))
  (dispatcher-api:shutdown (dispatcher self)))
