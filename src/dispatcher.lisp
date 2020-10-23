(defpackage :cl-gserver.dispatcher
  (:use :cl :cl-gserver.actor :cl-gserver.dispatcher-api)
  (:nicknames :dispatcher)
  (:import-from #:mesgb
                #:message-box-bt)
  (:export #:shared-dispatcher
           #:make-dispatcher
           #:make-dispatcher-worker
           #:dispatcher-workers))

(in-package :cl-gserver.dispatcher)

(defun make-dispatcher (dispatcher-type &key (num-workers 1))
  (make-instance dispatcher-type
                 :num-workers num-workers))

(defclass dispatcher-base ()
  ((num-workers :initarg :num-workers
                :initform 1
                :type integer
                :reader num-workers
                :documentation "The desired number of workers.")
   (workers :initform nil
            :reader dispatcher-workers
            :documentation "The worker instances"))
  (:documentation
   "A `dispatcher' is a pool of `actors'. The dispatching to its actors works in a random way."))

(defmethod print-object ((obj dispatcher-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (num-workers) obj
      (format stream "num-workers: ~a" num-workers))))

(defmethod initialize-instance :after ((self dispatcher-base) &key)
  (with-slots (num-workers workers worker-creator-fun) self
    (setf workers 
          (loop for x from 1 to num-workers
                collect (make-dispatcher-worker x)))))

(defgeneric dispatch (dispatcher dispatch-exec-fun))
(defgeneric dispatch-async (dispatcher dispatch-exec-fun))
(defgeneric shutdown (dispatcher))
(defmethod shutdown ((self dispatcher-base))
  "Stops all workers."
  (with-slots (workers) self
    (mapcar (lambda (worker) (tell worker :stop)) workers)))


;; ---------------------------------
;; Shared dispatcher
;; ---------------------------------

(defclass shared-dispatcher (dispatcher-base) ())

(defmethod dispatch ((self shared-dispatcher) dispatch-exec-fun)
  "Dispatches a function (`dispatch-exec-fun') to a worker of the dispatcher to execute there.
`dispatch' does a `ask' to a `dispatcher' worker, which means this call will block.
The strategy to select a worker is random."
  (with-slots (workers num-workers) self
    (when workers
      (ask (nth (random num-workers) workers) (cons :execute dispatch-exec-fun)))))

(defmethod dispatch-async ((self shared-dispatcher) dispatch-exec-fun)
  "Dispatches a function to a worker of the dispatcher to execute there.
`dispatch-async' does a `tell' to a `dispatcher' worker and is asynchronous.
The strategy to select a worker is random."
  (with-slots (workers num-workers) self
    (when workers
      (tell (nth (random num-workers) workers) (cons :execute dispatch-exec-fun)))))


;; ---------------------------------
;; the worker
;; ---------------------------------

(defclass dispatch-worker (actor) ()
  (:documentation
   "Specialized `actor' used as `worker' is the message `dispatcher'."))

(defun make-dispatcher-worker (num)
  (let ((worker (make-instance 'dispatch-worker
                               :receive-fun #'receive-fun
                               :name (utils:mkstr "dispatch-worker-" num)
                               :msgbox (make-instance 'message-box-bt
                                                      :max-queue-size 1000))))
    worker))

(defun receive-fun (self message current-state)
  "The worker receive function."
  (declare (ignore self))
  (case (car message)
    (:execute (cons (funcall (cdr message)) current-state))))
