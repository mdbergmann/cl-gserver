(defpackage :cl-gserver.dispatcher
  (:use :cl :cl-gserver :cl-gserver.dispatcher-api)
  (:nicknames :dispatcher)
  (:export #:dispatcher-bt))

(in-package :cl-gserver.dispatcher)

(defclass dispatcher-bt (dispatcher-base) ())

(defmethod initialize-instance :after ((self dispatcher-bt) &key)
  (with-slots (num-workers workers) self
    (setf workers 
          (loop for x from 1 to num-workers
                collect (make-instance 'dispatch-worker
                                       :name (utils:mkstr "dispatch-worker-" x)
                                       :max-queue-size 100)))))

(defmethod dispatch ((self dispatcher-bt) fun)
  (with-slots (workers num-workers) self
    (when workers
      (call (nth (random num-workers) workers) (cons :execute fun)))))

(defmethod dispatch-async ((self dispatcher-bt) fun)
  (with-slots (workers num-workers) self
    (when workers
      (cast (nth (random num-workers) workers) (cons :execute fun)))))

(defmethod shutdown ((self dispatcher-bt))
  (with-slots (workers) self
    (mapcar (lambda (worker) (cast worker :stop)) workers)))

;; ---------------------------------
;; the worker
;; ---------------------------------

(defclass dispatch-worker (gserver) ())
(defmethod handle-cast ((self dispatch-worker) message current-state)
  (case (car message)
    (:execute (cons (funcall (cdr message)) current-state))))
(defmethod handle-call ((self dispatch-worker) message current-state)
  (case (car message)
    (:execute (cons (funcall (cdr message)) current-state))))
