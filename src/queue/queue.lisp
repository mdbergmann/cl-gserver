(defpackage :cl-gserver.queue
  (:use :cl)
  (:nicknames :queue)
  (:export #:queue-unbounded
           #:queue-bounded
           #:pushq
           #:popq
           #:emptyq-p))

(in-package :cl-gserver.queue)

(defclass queue-base ()
  ((queue :initform nil))
  (:documentation "The base queue."))

(defgeneric pushq (queue-base element)
  (:documentation "Pushes an element to the queue."))

(defgeneric popq (queue-base)
  (:documentation "Pops the first element. Blocks until an element arrives."))

(defgeneric emptyq-p (queue-base)
  (:documentation "Returns `T' if there is no element in the queue."))

;; ----------------------------------------
;; ----------- lparallel cons-queue -------
;; ----------------------------------------

(defclass queue-unbounded (queue-base)
  ((queue :initform (lparallel.cons-queue:make-cons-queue)))
  (:documentation "Unbounded queue based on lparallels cons-queue."))

(defmethod pushq ((self queue-unbounded) element)
  (with-slots (queue) self
    (lparallel.cons-queue:push-cons-queue element queue)))

(defmethod popq ((self queue-unbounded))
  (with-slots (queue) self
    (lparallel.cons-queue:pop-cons-queue queue)))

(defmethod emptyq-p ((self queue-unbounded))
  (with-slots (queue) self
    (lparallel.cons-queue:cons-queue-empty-p queue)))

;; ----------------------------------------
;; ----------- cl-speedy-queue ------------
;; ----------------------------------------

(defclass queue-bounded (queue-base)
  ((queue :initform nil)
   (lock :initform (bt:make-lock))
   (cvar :initform (bt:make-condition-variable))
   (max-items :initform 1000 :initarg :max-items)
   (yield-threshold :initform nil))
  (:documentation "Bounded queue."))

(defmethod initialize-instance :after ((self queue-bounded) &key)
  (with-slots (queue max-items yield-threshold) self
    (if (< max-items 0) (error "Max-items 0 or less is not allowed!"))

    (setf yield-threshold
          (cond
            ((<= max-items 2) 0)
            ((<= max-items 10) 2)
            ((<= max-items 20) 8)
            (t (* (/ max-items 100) 95))))  ; 95%
    (log:info "Yield threshold at: " yield-threshold)

    (setf queue (cl-speedy-queue:make-queue max-items))))

(defmethod pushq ((self queue-bounded) element)
  (with-slots (queue lock cvar yield-threshold) self

    (backpressure-if-necessary-on queue yield-threshold)

    (bt:with-lock-held (lock)
      (cl-speedy-queue:enqueue element queue)
      (bt:condition-notify cvar))))
    

(defun backpressure-if-necessary-on (queue yield-threshold)
  (loop :for queue-count = (get-queue-count queue)
        :for loop-count :from 0
        :if (and (> loop-count 100) (> queue-count yield-threshold))
          :do (progn
                (log:warn "Unable to reduce queue pressure!")
                (error "Unable to reduce queue pressure. Consider increasing queue-size or use more threads!"))
        :while (> queue-count yield-threshold)
        :do (progn
              (log:debug "back-pressure, doing thread-yield (~a/~a)." queue-count yield-threshold)
              (bt:thread-yield)
              (sleep .01))))

(defun get-queue-count (queue)
  (cl-speedy-queue:queue-count queue))

(defmethod popq ((self queue-bounded))
  (with-slots (queue lock cvar) self
    (bt:with-lock-held (lock)
      (log:debug "Lock aquired...")
      #-ccl (if (> (get-queue-count queue) 0)
                (dequeue/no-wait queue)
                (dequeue/wait queue cvar lock))
      #+ccl (dequeue/wait queue cvar lock)
      ;; this loop was suggested in a bordeaux-threads GitHub issue conversation.
      ;; but it doesn't work on CCL.
      ;; (progn
      ;;   (loop
      ;;     :while (emptyq-p self)
      ;;     :do (dequeue/wait queue cvar lock))
      ;;   (dequeue/no-wait queue))
      )))

(defun dequeue/no-wait (queue)
  (log:debug "Dequeue without wait...")
  (cl-speedy-queue:dequeue queue))

(defun dequeue/wait (queue cvar lock)
  (log:debug "Going to sleep...")
  (bt:condition-wait cvar lock)
  (log:debug "Awoken, processing queue...")
  (cl-speedy-queue:dequeue queue))

(defmethod emptyq-p ((self queue-bounded))
  (with-slots (queue) self
    (cl-speedy-queue:queue-empty-p queue)))
