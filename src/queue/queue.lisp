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
   (lock :initform (bt:make-lock))
   (cvar :initform (bt:make-condition-variable))
   (max-items :initform 1000 :initarg :max-items))
  (:documentation "Bounded queue."))

(defmethod initialize-instance :after ((self queue-bounded) &key)
  (with-slots (queue max-items) self
    (if (< max-items 0) (error "Max-items 0 or less is not allowed!"))
    (setf queue (cl-speedy-queue:make-queue max-items))))

(defmethod pushq ((self queue-bounded) element)
  (with-slots (queue lock cvar yield-threshold) self
    (bt:with-lock-held (lock)
      (when (cl-speedy-queue:queue-full-p queue)
        (error 'queue-full-error :queue self))
      (cl-speedy-queue:enqueue element queue)
      (bt:condition-notify cvar))))

(defmethod popq ((self queue-bounded))
  (with-slots (queue lock cvar) self
    (bt:with-lock-held (lock)
      (loop :while (cl-speedy-queue:queue-empty-p queue)
            :do (bt:condition-wait cvar lock)
            :finally (return (cl-speedy-queue:dequeue queue))))))

(defmethod emptyq-p ((self queue-bounded))
  (with-slots (queue) self
    (cl-speedy-queue:queue-empty-p queue)))

(defmethod queued-count ((self queue-bounded))
  (cl-speedy-queue:queue-count self))
