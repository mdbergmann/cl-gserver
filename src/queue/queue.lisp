(defpackage :sento.queue
  (:use :cl)
  (:nicknames :queue)
  (:export #:queue-unbounded
           #:queue-bounded
           #:pushq
           #:popq
           #:emptyq-p
           #:queued-count
           ;; conditions
           #:queue-full-error))

(in-package :sento.queue)

(defclass queue-base ()
  ()
  (:documentation "The base queue."))

(defgeneric pushq (queue-base element)
  (:documentation "Pushes an element to the queue."))

(defgeneric popq (queue-base)
  (:documentation "Pops the first element. Blocks until an element arrives."))

(defgeneric emptyq-p (queue-base)
  (:documentation "Returns `T' if there is no element in the queue."))

(defgeneric queued-count (queue-base)
  (:documentation "Returns the number of elements in the queue."))

;;
;; unbounded queues in separate files
;;

;; ----------------------------------------
;; --- Bounded-queue - cl-speedy-queue ----
;; ----------------------------------------

(define-condition queue-full-error (error)
  ((queue :initarg :queue :reader queue))
  (:report (lambda (condition stream)
             (format stream "Queue '~a' is full!" (queue condition)))))

(defclass queue-bounded (queue-base)
  ((queue :initform nil)
   (lock :initform (bt2:make-lock))
   (cvar :initform (bt2:make-condition-variable))
   (max-items :initform 1000 :initarg :max-items)
   (fill-count :initform 0)) ; cl-speedy-queue has issues with queued items count
  (:documentation "Bounded queue."))

(defmethod initialize-instance :after ((self queue-bounded) &key)
  (with-slots (queue max-items) self
    (if (< max-items 0) (error "Max-items 0 or less is not allowed!"))
    (setf queue (cl-speedy-queue:make-queue max-items))))

(defmethod pushq ((self queue-bounded) element)
  (with-slots (queue lock cvar fill-count max-items) self
    (when (>= fill-count max-items)
      (error 'queue-full-error :queue self))
    (bt2:with-lock-held (lock)
      (cl-speedy-queue:enqueue element queue)
      (incf fill-count)
      (bt2:condition-notify cvar))))

(defmethod popq ((self queue-bounded))
  (with-slots (queue lock cvar) self
    (bt2:with-lock-held (lock)
      (loop :while (cl-speedy-queue:queue-empty-p queue)
            :do (bt2:condition-wait cvar lock)
            :finally (return
                       (progn
                         (decf (slot-value self 'fill-count))
                         (cl-speedy-queue:dequeue queue)))))))

(defmethod emptyq-p ((self queue-bounded))
  (with-slots (queue) self
    (cl-speedy-queue:queue-empty-p queue)))

(defmethod queued-count ((self queue-bounded))
  (slot-value self 'fill-count))
