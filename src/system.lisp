(defpackage :cl-gserver.system
  (:use :cl :cl-gserver)
  (:nicknames :system)
  (:import-from :dispatcher
                #:dispatcher-bt)
  (:import-from :dispatcher-api
                #:make-dispatcher
                #:terminate)
  (:export #:make-system
           #:system
           #:dispatcher
           #:terminate))

(in-package :cl-gserver.system)

(defclass system ()
  ((dispatcher :initarg :dispatcher
               :initform (make-instance 'dispatcher-bt)
               :accessor dispatcher
               :documentation "The message dispatcher."))
  (:documentation
"A system is a container for GServers/Actors or other incarnations of GServer."))

(defmethod print-object ((obj system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatcher) obj
      (format stream "dispatcher: ~a"
              dispatcher))))


(defun make-system (&key (num-workers 1))
  (make-instance 'system :dispatcher (make-dispatcher 'dispatcher-bt :num-workers num-workers)))

(defun terminate (system)
  (dispatcher-api:terminate (dispatcher system)))
