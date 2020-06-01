(defpackage :cl-gserver.queue
  (:use :cl :lparallel.queue)
  (:nicknames :queue)
  (:export #:queue-lcc
           #:pushq
           #:popq))

(in-package :cl-gserver.queue)

(defclass queue-base ()
  ((queue :initform nil))
  (:documentation "The base queue."))

(defgeneric pushq (queue-base element)
  (:documentation "Pushes an element to the queue."))

(defgeneric popq (queue-base)
  (:documentation "Pops the first element. Blocks until an element arrives."))


;; ----------------------------------------
;; ----------- lparallel cons-queue -------
;; ----------------------------------------

(defclass queue-lcc (queue-base)
  ((queue :initform (lparallel.cons-queue:make-cons-queue))))

(defmethod pushq ((self queue-lcc) element)
  (with-slots (queue) self
    (lparallel.cons-queue:push-cons-queue element queue)))

(defmethod popq ((self queue-lcc))
  (with-slots (queue) self
    (lparallel.cons-queue:pop-cons-queue queue)))
