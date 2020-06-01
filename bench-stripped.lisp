;;(ql:quickload "lparallel")
;;(ql:quickload "bordeaux-threads")

(use-package :lparallel)

(defparameter *msgbox-kernel* nil)
(defparameter *msgbox-channel* nil)
(defparameter *counter* 0)
(defparameter *called* 0)
(defparameter +threads+ 8)
(defparameter +per-thread+ 50)

(defun max-loop () (* +per-thread+ +threads+))

(format t "Times: ~a~%" (max-loop))

(defun submit-msg-sr ()
  (let* ((lparallel:*kernel* *msgbox-kernel*)
         (channel *msgbox-channel*))
    (lparallel:submit-task channel
                           (lambda ()
                             (1+ *counter*)))
    (setf *counter* (lparallel:receive-result channel))))

(defun submit-msg-fut ()
  (let* ((lparallel:*kernel* *msgbox-kernel*)
         (fut (lparallel:future
                (incf *counter*)
                *counter*)))
    (force fut)))

(defun runner (submit-fun)
  (setf *msgbox-kernel* (lparallel:make-kernel 1))
  (let ((lparallel:*kernel* *msgbox-kernel*))
    (setf *msgbox-channel* (lparallel:make-channel)))
  (setf lparallel:*kernel* (lparallel:make-kernel +threads+))
  (setf *counter* 0)

  (unwind-protect
       (time
        (map nil #'lparallel:force
             (mapcar (lambda (n)
                       (lparallel:future
                         (dotimes (n +per-thread+)
                           (funcall submit-fun))))
                     (loop for n from 1 to +threads+ collect n))))
    (format t "Counter: ~a~%" *counter*)
    (sleep 1)
    (format t "Counter: ~a~%" *counter*)

    (lparallel:end-kernel :wait t)
    (let ((lparallel:*kernel* *msgbox-kernel*))
      (lparallel:end-kernel :wait t))))

(defun runner-sr ()
  (runner #'submit-msg-sr))
(defun runner-fut ()
  (runner #'submit-msg-fut))

