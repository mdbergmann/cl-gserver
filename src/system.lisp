(defpackage :cl-gserver.system
  (:use :cl :cl-gserver.actor :cl-gserver.system-actor
        :cl-gserver.system-api :cl-gserver.actor-context)
  (:nicknames :system)
  (:import-from #:dispatcher-api
                #:make-dispatcher)
  (:import-from #:dispatcher
                #:dispatcher-bt)
  (:export #:make-system
           #:system))

(in-package :cl-gserver.system)

(defclass system (actor-context)
  ((dispatcher :initarg :dispatcher
               :initform nil
               :reader get-dispatcher
               :documentation "The message dispatcher."))
  (:documentation
   "A system is a container for `actors' or subclasses."))

(defmethod print-object ((obj system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatcher) obj
      (format stream "dispatcher: ~a"
              dispatcher))))

(defun make-system (&key (num-workers 4))
  "Creates a system with 4 workers by default."
  (make-instance 'system
                 :dispatcher (make-dispatcher 'dispatcher-bt :num-workers num-workers)))

(defmethod shutdown ((self system))
  (dispatcher-api:shutdown (get-dispatcher self)))

;; -------------------------------------
;; actor-context impl
;; -------------------------------------

(defmethod actor-of ((self system) create-fun)
  (let ((system-actor (make-system-actor self create-fun)))
    (add-actor self system-actor)
    system-actor))
