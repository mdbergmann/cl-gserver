(defpackage :cl-gserver.dispatcher-api
  (:use :cl)
  (:nicknames :dispatcher-api)
  (:export #:make-dispatcher
           #:terminate
           #:dispatch
           #:dispatch-async
           #:workers
           ))

(in-package :cl-gserver.dispatcher-api)

(defun make-dispatcher (dispatcher-type &key (num-workers 1))
  (make-instance dispatcher-type :num-workers num-workers))

(defclass dispatcher-base ()
  ((num-workers :initarg :num-workers
                :initform 1
                :type integer
                :documentation "The desired number of workers.")
   (workers :initform nil
            :reader workers
            :documentation "The worker instances")))

(defmethod print-object ((obj dispatcher-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (num-workers workers) obj
      (format stream "num-workers: ~a, workers: ~a"
              num-workers
              workers))))

(defgeneric terminate (dispatcher-base))
(defgeneric dispatch (dispatcher-base fun))
(defgeneric dispatch-async (dispatcher-base fun))
