(in-package :sento.queue)

(eval-when (:compile-toplevel)
  (require :sb-concurrency)
  (shadowing-import '(sb-concurrency:make-queue
                      sb-concurrency:enqueue
                      sb-concurrency:dequeue
                      sb-concurrency:queue-empty-p)))

;; ----------------------------------------
;; ---- unbounded queue - sbcl only -------
;; uses the lock-free (based on cas) queue
;; ----------------------------------------

(defclass queue-unbounded (queue-base)
  ((queue :initform
          (make-queue)))
  (:documentation "Unbounded queue."))

(defmethod pushq ((self queue-unbounded) element)
  (with-slots (queue) self
    (enqueue element queue)))

(defmethod popq ((self queue-unbounded))
  (with-slots (queue) self
    (dequeue queue)))

(defmethod emptyq-p ((self queue-unbounded))
  (with-slots (queue) self
    (queue-empty-p queue)))
