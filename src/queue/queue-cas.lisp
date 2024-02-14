(in-package :sento.queue)

;; WIP

;; ----------------------------------------
;; - unbounded queue using cas (lock-free)
;; ----------------------------------------

#|
Implementation copied and adapted from SBCL's queue.lisp
|#

(defconstant +dummy+ '.dummy.)
(defconstant +dead-end+ '.dead-end.)

(defstruct (queue
            (:constructor %make-queue (head tail))
            (:copier nil)
            (:predicate queueq))
  (head (error "No head"))
  (tail (error "No tail")))

(defun make-queue ()
  (let* ((dummy (cons +dummy+ nil))
         (queue (%make-queue
                 (atomic:make-atomic-reference :value dummy)
                 (atomic:make-atomic-reference :value dummy))))
    (flet ((enc-1 (x)
             (enqueue x queue)))
      (declare (dynamic-extent #'enc-1))
      (map nil #'enc-1 nil))
    queue))

(defun enqueue (item queue)
  (declare (optimize speed))
  (let ((new (cons item nil)))
    (atomic:atomic-swap (queue-tail queue)
                        (lambda (old)
                          (setf (cdr old) new)
                          new))
    (setf (queue-tail queue) (atomic:make-atomic-reference :value new))
    item))

(defun dequeue (queue)
  (declare (optimize speed))
  (let ((next (atomic:atomic-swap
               (queue-head queue)
               (lambda (head)
                 (let ((next (cdr head)))
                   ;;(print next)
                   (typecase next
                     (null :end) ;; break cas
                     (cons next)))))))
    ;;(print next)
    (when (eq next :end)
      (return-from dequeue (values nil nil)))
    (let ((item (car next)))
      (setf ;;(cdr head) +dead-end+
            (car next) +dummy+)
      (values item t))))

  
  ;; (let* ((head (atomic:atomic-get (queue-head queue)))
  ;;        (next (cdr head)))
  ;;   (typecase next
  ;;     (null (values nil nil))
  ;;     (cons
  ;;      (atomic:atomic-swap
  ;;       (queue-head queue)
  ;;       (lambda () next))
  ;;      (let ((item (car next)))
  ;;        (setf (cdr head) +dead-end+
  ;;              (car next) +dummy+)
  ;;        (values item t))))))

(defun emptyp (queue)
  (null (cdr (atomic:atomic-get (queue-head queue)))))


;; ----------------------------------------

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
