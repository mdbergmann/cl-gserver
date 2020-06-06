(defpackage :cl-gserver.queue
  (:use :cl)
  (:nicknames :queue)
  (:local-nicknames (:lparq :lparallel.cons-queue)
                    (:speedq :cl-speedy-queue))
  (:export #:queue-unbounded
           #:queue-bounded
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

(defclass queue-unbounded (queue-base)
  ((queue :initform (lparq:make-cons-queue)))
  (:documentation "Unbounded queue based on lparallels cons-queue."))

(defmethod pushq ((self queue-unbounded) element)
  (with-slots (queue) self
    (lparq:push-cons-queue element queue)))

(defmethod popq ((self queue-unbounded))
  (with-slots (queue) self
    (lparq:pop-cons-queue queue)))


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
            (t (* (/ max-items 100) 90))))  ; 90%
    (log:info "Yield threshold at: " yield-threshold)

    (setf queue (speedq:make-queue max-items))))

(defmethod pushq ((self queue-bounded) element)
  (with-slots (queue lock cvar yield-threshold) self

    (backpressure-if-necessary-on queue yield-threshold)

    (bt:with-lock-held (lock)
      (speedq:enqueue element queue)
      (bt:condition-notify cvar))))
    

(defun backpressure-if-necessary-on (queue yield-threshold)
  (iter:iter (iter:for count
                       initially (get-queue-count queue)
                       then (get-queue-count queue))
    (log:debug "Queue size (push): " count)
    (if (> count yield-threshold)
        (progn
          (log:debug "back-pressure, doing thread-yield.")
          (bt:thread-yield)
          ;;(sleep .01)
          ))
    (iter:while (> count yield-threshold))))

(defun get-queue-count (queue)
  (speedq:queue-count queue))

(defmethod popq ((self queue-bounded))
  (with-slots (queue lock cvar) self
    (bt:with-lock-held (lock)
      (log:debug "Lock aquired...")
      #-ccl (if (> (get-queue-count queue) 0)
                (dequeue/no-wait queue)
                (dequeue/wait queue cvar lock))
      #+ccl (dequeue/wait queue cvar lock)
      )))

(defun dequeue/no-wait (queue)
  (log:debug "Dequeue without wait...")
  (speedq:dequeue queue))

(defun dequeue/wait (queue cvar lock)
  (log:debug "Going to sleep...")
  (bt:condition-wait cvar lock)
  (log:debug "Awoken, processing queue...")
  (speedq:dequeue queue))
