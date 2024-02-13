(in-package :sento.queue)

;; WIP

;; ----------------------------------------
;; - unbounded queue using cas (lock-free)
;; ----------------------------------------

#|
The queue is a simple queue that is not thread-safe.
It is based on 2 stacks, one for the head and one for the tail.
When the tail is empty, the head is reversed and pushed to the tail.
This is from the book "Programming Algorithms in Lisp" by Vsevolod Domkin.

This queue is fast, but requires a lot of memory. Roughly 1/3 more
than the 'queue' implementation of lparallel.
|#

(defstruct queue
  (head (atomic:make-atomic-reference :value '()))
  (tail (atomic:make-atomic-reference :value '())))

(defun enqueue (item queue)
  (atomic:atomic-swap
   (queue-head queue)
   (lambda (lst) (cons item lst))))

(defun dequeue (queue)
  (declare (optimize
            (speed 3)
            (safety 0)
            (debug 0)
            (compilation-speed 0)))
  (unless (atomic:atomic-get (queue-tail queue))
    (do ()
        ((null (atomic:atomic-get (queue-head queue))))
      (loop
        (let ((qhead (atomic:atomic-get (queue-head queue)))
              (qtail (atomic:atomic-get (queue-tail queue))))
          (destructuring-bind (head . tail) qhead
            (when (and
                   (atomic:atomic-cas (queue-head queue)
                                      qhead tail)
                   (atomic:atomic-cas (queue-tail queue)
                                      qtail
                                      (cons head qtail)))
              (return)))))))
  (when (atomic:atomic-get (queue-tail queue))
    (let* ((qtail (atomic:atomic-get (queue-tail queue)))
           (head (car qtail)))
      (atomic:atomic-swap (queue-tail queue)
                          (lambda (lst) (cdr lst)))
      (values head t))))

(defun emptyp (queue)
  (not (or (queue-head queue)
           (queue-tail queue))))


#|
queue implementation from lparallel.
Copyright (c) 2011-2012, James M. Lawrence. All rights reserved.

|#

;; (defstruct queue
;;   (head '() :type list)
;;   (tail '() :type list))

;; (defun enqueue (item queue)
;;   (declare (optimize
;;             (speed 3) (safety 0) (debug 0)
;;             (compilation-speed 0)))
;;   (let ((new (cons item nil)))
;;     (if (queue-head queue)
;;         (setf (cdr (queue-tail queue)) new)
;;         (setf (queue-head queue) new))
;;     (setf (queue-tail queue) new)))

;; (defun dequeue (queue)
;;   (declare (optimize
;;             (speed 3) (safety 0) (debug 0)
;;             (compilation-speed 0)))
;;   (let ((item (queue-head queue)))
;;     (if item
;;         (multiple-value-prog1 (values (car item) t)
;;           (when (null (setf (queue-head queue) (cdr item)))
;;             (setf (queue-tail queue) nil))
;;           ;; clear item for conservative gcs
;;           (setf (car item) nil
;;                 (cdr item) nil))
;;         (values nil nil))))

;; (defun emptyp (queue)
;;   (not (queue-head queue)))

;; ------- thread-safe queue --------

(defclass queue-unbounded (queue-base)
  ((queue :initform (make-queue)))
  (:documentation "Unbounded queue."))

(defmethod pushq ((self queue-unbounded) element)
  (with-slots (queue) self
    (enqueue element queue)))

(defmethod popq ((self queue-unbounded))
  (with-slots (queue) self
      (loop (multiple-value-bind (value presentp)
                (dequeue queue)
              (if presentp
                  (return value))))))

(defmethod emptyq-p ((self queue-unbounded))
  (with-slots (queue) self
    (emptyp queue)))
