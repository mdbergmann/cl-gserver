(defpackage :cl-gserver.dispatcher-api
  (:use :cl)
  (:nicknames :dispatcher-api)
  (:export #:make-dispatcher
           #:dispatcher-base
           #:shutdown
           #:dispatch
           #:dispatch-async
           #:workers
           #:num-workers
           ))

(in-package :cl-gserver.dispatcher-api)

(defun make-dispatcher (dispatcher-type &key (num-workers 1))
  (make-instance dispatcher-type :num-workers num-workers))

(defclass dispatcher-base ()
  ((num-workers :initarg :num-workers
                :initform 1
                :type integer
                :reader num-workers
                :documentation "The desired number of workers.")
   (workers :initform nil
            :reader workers
            :documentation "The worker instances")))

(defmethod print-object ((obj dispatcher-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (num-workers workers) obj
      (format stream "num-workers: ~a" num-workers))))

(defgeneric shutdown (dispatcher-base))
(defgeneric dispatch (dispatcher-base fun))
(defgeneric dispatch-async (dispatcher-base fun))
