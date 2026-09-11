(defpackage :sento.queue
  (:use :cl)
  (:nicknames :queue)
  (:export #:queue-unbounded
           #:queue-bounded
           #:pushq
           #:popq
           #:try-popq
           #:try-popq-into
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

(defgeneric try-popq (queue-base)
  (:documentation "Pops the first element without blocking.
Returns `(values element t)` when an element was available and
`(values nil nil)` when the queue is empty."))

(defgeneric try-popq-into (queue-base vector)
  (:documentation "Pops up to `(length vector)` elements into `vector`, starting
at index 0, without blocking and under one acquisition of the queue lock.
Returns the number of elements popped, 0 when the queue is empty."))

(defgeneric emptyq-p (queue-base)
  (:documentation "Returns `T` if there is no element in the queue.
Takes the queue lock, so an element pushed by another thread before the call
is accounted for."))

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
   (waiters :initform 0
            :documentation
            "Number of threads blocked in `popq`. Maintained under the lock;
a push only notifies the condition-variable when it is positive.")
   (max-items :initform 1000 :initarg :max-items)
   (fill-count :initform 0)) ; cl-speedy-queue has issues with queued items count
  (:documentation "Bounded queue."))

(defmethod initialize-instance :after ((self queue-bounded) &key)
  (with-slots (queue max-items) self
    (if (< max-items 0) (error "Max-items 0 or less is not allowed!"))
    (setf queue (cl-speedy-queue:make-queue max-items))))

(defmethod pushq ((self queue-bounded) element)
  (with-slots (queue lock cvar waiters fill-count max-items) self
    (let ((notify-p nil))
      (bt2:with-lock-held (lock)
        (when (>= fill-count max-items)
          (error 'queue-full-error :queue self))
        (cl-speedy-queue:enqueue element queue)
        (incf fill-count)
        (setf notify-p (plusp waiters)))
      ;; notified after the lock is released so that the woken consumer does
      ;; not immediately block on the lock this thread still holds.
      (when notify-p
        (bt2:condition-notify cvar)))))

(defmethod popq ((self queue-bounded))
  (with-slots (queue lock cvar waiters fill-count) self
    (bt2:with-lock-held (lock)
      (loop :while (cl-speedy-queue:queue-empty-p queue)
            :do (incf waiters)
                (unwind-protect
                     (bt2:condition-wait cvar lock)
                  (decf waiters))
            :finally (return
                       (progn
                         (decf fill-count)
                         (cl-speedy-queue:dequeue queue)))))))

(defmethod try-popq ((self queue-bounded))
  (with-slots (queue lock fill-count) self
    (bt2:with-lock-held (lock)
      (if (cl-speedy-queue:queue-empty-p queue)
          (values nil nil)
          (progn
            (decf fill-count)
            (values (cl-speedy-queue:dequeue queue) t))))))

(defmethod try-popq-into ((self queue-bounded) vector)
  (with-slots (queue lock fill-count) self
    (bt2:with-lock-held (lock)
      (let ((count 0))
        (loop :while (and (< count (length vector))
                          (not (cl-speedy-queue:queue-empty-p queue)))
              :do (setf (aref vector count) (cl-speedy-queue:dequeue queue))
                  (incf count))
        (decf fill-count count)
        count))))

(defmethod emptyq-p ((self queue-bounded))
  (with-slots (queue lock) self
    (bt2:with-lock-held (lock)
      (cl-speedy-queue:queue-empty-p queue))))

(defmethod queued-count ((self queue-bounded))
  (slot-value self 'fill-count))
