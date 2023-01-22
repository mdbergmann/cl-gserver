(eval-when (:compile-toplevel)
  (ql:quickload '(:lparallel :jpl-queues)))

(defstruct queue-obj a b c)

(defparameter *sync-unbounded-queue*
  (make-instance 'jpl-queues:synchronized-queue
                 :queue 
                 (make-instance 'jpl-queues:unbounded-fifo-queue)))
(defparameter *sync-bounded-queue*
  (make-instance 'jpl-queues:synchronized-queue
                 :queue 
                 (make-instance 'jpl-queues:bounded-fifo-queue 
                                :capacity 1000000)))
(defun queue-and-dequeue (queue)
  (loop :repeat 1000000
        :do
           (progn
             (jpl-queues:enqueue 
              (make-queue-obj :a 1 :b 2 :c 3)
              queue)
             (jpl-queues:dequeue queue))))

(defparameter *cons-queue* (lparallel.cons-queue:make-cons-queue))
(defun queue-and-dequeue-cq (queue)
  (loop :repeat 1000000
        :do
           (progn
             (lparallel.cons-queue:push-cons-queue 
              (make-queue-obj :a 1 :b 2 :c 3)
              queue)
             (lparallel.cons-queue:pop-cons-queue queue))))

(defun run-tests ()
  (format t "jpl-unbounded:~%")
  (time (queue-and-dequeue *sync-unbounded-queue*))
  (format t "jpl-bounded:~%")
  (time (queue-and-dequeue *sync-bounded-queue*))
  (format t "cons-queue unbounded:~%")
  (time (queue-and-dequeue-cq *cons-queue*)))
