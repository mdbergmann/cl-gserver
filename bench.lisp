(asdf:load-system :lparallel)
(use-package :lparallel)

(asdf:load-system :cl-speedy-queue)
(use-package :cl-speedy-queue)

(push #P"~/Development/MySources/cl-gserver/" asdf:*central-registry*)
(asdf:load-system "cl-gserver")

;;(use-package :bordeaux-threads)

(defparameter *starttime* 0)
(defparameter *endtime* 0)

(defparameter *withreply-p* nil)

(defparameter *msgbox* nil)
(defparameter *counter* 0)
(defparameter +threads+ 8)
(defparameter +per-thread+ 1000000)

(defun max-loop () (* +per-thread+ +threads+))

(format t "Times: ~a~%" (max-loop))

(defun msg-submit ()
  (cl-gserver.messageb:with-submit-handler (*msgbox* :foo *withreply-p*)
                                           (progn
                                             (incf *counter*)
                                             *counter*)))

(defun assert-cond (assert-fun max-time)
  (do ((wait-time 0.02 (+ wait-time 0.02))
       (fun-result nil (funcall assert-fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep 0.02))))

(defun runner-bt-lsr ()
  (setf *msgbox* (make-instance 'cl-gserver.messageb::message-box-lsr))
  (setf *withreply-p* t)
  (setf *counter* 0)
  (setf *starttime* (get-universal-time))
  (time
   (progn
     (map nil #'bt:join-thread
          (mapcar (lambda (x)
                    (bt:make-thread (lambda ()
                                      (dotimes (n +per-thread+)
                                        (msg-submit)))
                                    :name x))
                  (mapcar (lambda (n) (format nil "thread-~a" n))
                          (loop for n from 1 to +threads+ collect n))))
     (assert-cond (lambda () (= *counter* (max-loop))) 5)))
  (setf *endtime* (get-universal-time))
  (format t "Counter: ~a~%" *counter*)
  (format t "Elapsed: ~a~%" (- *endtime* *starttime*))
  (cl-gserver.messageb:stop *msgbox*))

(defun runner-bt-bt (&optional (withreply-p nil))
  (setf *msgbox* (make-instance 'cl-gserver.messageb::message-box-bt))
  (setf *withreply-p* withreply-p)
  (setf *counter* 0)
  (setf *starttime* (get-universal-time))
  (time
   (progn
     (map nil #'bt:join-thread
          (mapcar (lambda (x)
                    (bt:make-thread (lambda ()
                                      (dotimes (n +per-thread+)
                                        (msg-submit)))
                                    :name x))
                  (mapcar (lambda (n) (format nil "thread-~a" n))
                          (loop for n from 1 to +threads+ collect n))))
     (assert-cond (lambda () (= *counter* (max-loop))) 5)))
  (setf *endtime* (get-universal-time))
  (format t "Counter: ~a~%" *counter*)
  (format t "Elapsed: ~a~%" (- *endtime* *starttime*))
  (cl-gserver.messageb:stop *msgbox*))




;; (defun runner-lp ()
;;   (setf *msgbox* (make-instance 'cl-gserver.messageb::message-box-lsr))
;;   (setf lparallel:*kernel* (lparallel:make-kernel +threads+))
;;   (setf *counter* 0)

;;   (unwind-protect
;;        (time
;;         (let ((chan (lparallel:make-channel)))
;;           (dotimes (n (max-loop))
;;             (lparallel:submit-task chan #'msg-submit))
;;           (dotimes (n (max-loop))
;;             (lparallel:receive-result chan))))
;;     (format t "Counter: ~a~%" *counter*)
;;     (lparallel:end-kernel)
;;     (cl-gserver.messageb::stop *msgbox*)))

;; (defun runner-lp2 ()
;;   (setf *msgbox* (make-instance 'cl-gserver.messageb::message-box-lsr))
;;   (setf lparallel:*kernel* (lparallel:make-kernel +threads+))
;;   (setf *counter* 0)

;;   (unwind-protect
;;        (time
;;         (progn 
;;           (map nil #'lparallel:force
;;                (mapcar (lambda (x)
;;                          (lparallel:future
;;                            (dotimes (n +per-thread+)
;;                              (msg-submit))))
;;                        (mapcar (lambda (n) (format nil "thread-~a" n))
;;                                (loop for n from 1 to +threads+ collect n))))
;;           (format t "Counter: ~a~%" *counter*)
;;           (assert-cond (lambda () (= *counter* (max-loop))) 5)))
;;     (format t "Counter: ~a~%" *counter*)
;;     (lparallel:end-kernel)
;;     (cl-gserver.messageb::stop *msgbox*)))

;; (defun runner-lp3 ()
;;   (setf *msgbox* (make-instance 'cl-gserver.messageb::message-box-lsr))
;;   (setf lparallel:*kernel* (lparallel:make-kernel +threads+))
;;   (setf *counter* 0)

;;   (unwind-protect
;;        (time
;;         (lparallel:pmap nil (lambda (per-thread)
;;                               (dotimes (n per-thread)
;;                                 (msg-submit)))
;;                         :parts 1
;;                         (loop repeat +threads+ collect +per-thread+)))
;;     (format t "Counter: ~a~%" *counter*)
;;     (lparallel:end-kernel)
;;     (cl-gserver.messageb::stop *msgbox*)))
