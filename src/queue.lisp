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
  ((queue :initform (speedq:make-queue 1000))
   (lock :initform (bt:make-lock))
   (cvar :initform (bt:make-condition-variable))
   (yield-threshold :initarg :yield-t
                    :initform 900))
  (:documentation "Bounded queue with 1000 elements."))

(defmethod pushq ((self queue-bounded) element)
  (with-slots (queue lock cvar yield-threshold) self
    (bt:acquire-lock lock t)
    (speedq:enqueue element queue)
    (bt:condition-notify cvar)
    (bt:release-lock lock)

    (let ((count (speedq:queue-count queue)))
      (log:debug "Queue size (push): " count)    
      (if (> count yield-threshold)
          (progn
            (log:debug "Threshold reached: " count)
            ;; we need to back-pressure
            (sleep 0.001)
            (bt:thread-yield))))))
    

(defmethod popq ((self queue-bounded))
  (with-slots (queue lock cvar) self
    (bt:with-lock-held (lock)
      (log:debug "Lock aquired...")
      (let ((count (speedq:queue-count queue)))
        (log:debug "Queue size: " count)
        (if (> count 0)
            (progn
              (log:debug "Returning popped item.")
              (speedq:dequeue queue))
            (progn
              (log:debug "Going to sleep...")
              (bt:condition-wait cvar lock)
              (log:debug "Awoken, processing queue...")
              (speedq:dequeue queue)))))))

