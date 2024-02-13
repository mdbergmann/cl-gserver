(in-package :sento.queue)

(eval-when (:compile-toplevel)
  (require :sb-concurrency))

;; ----------------------------------------
;; ---- unbounded queue - sbcl only -------
;; uses the lock-free (based on cas) queue
;; ----------------------------------------

(defclass queue-unbounded (queue-base)
  ((queue :initform
          (sb-concurrency:make-queue)))
  (:documentation "Unbounded queue."))

(defmethod pushq ((self queue-unbounded) element)
  (with-slots (queue) self
    (sb-concurrency:enqueue element queue)))

(defmethod popq ((self queue-unbounded))
  (with-slots (queue) self
    (sb-concurrency:dequeue queue)))

(defmethod emptyq-p ((self queue-unbounded))
  (with-slots (queue) self
    (sb-concurrency:queue-empty-p queue)))
