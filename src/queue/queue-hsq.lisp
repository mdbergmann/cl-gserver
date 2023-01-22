(in-package :sento.queue)

;; ----------------------------------------
;; ---- unbounded high speed queue --------
;; ----------------------------------------

;; based on lparallel cons-queue
;; this redefines the `queue-unbounded' implementation

(defclass queue-unbounded (queue-base)
  ((queue :initform
          (lparallel.cons-queue:make-cons-queue)))
  (:documentation "Unbounded queue."))

(defmethod pushq ((self queue-unbounded) element)
  (with-slots (queue) self
    (lparallel.queue:push-queue element queue)))

(defmethod popq ((self queue-unbounded))
  (with-slots (queue) self
    (lparallel.queue:pop-queue queue)))

(defmethod emptyq-p ((self queue-unbounded))
  (with-slots (queue) self
    (lparallel.queue:queue-empty-p queue)))
