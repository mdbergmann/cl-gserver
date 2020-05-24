(defpackage :cl-gserver.messageb
  (:use :cl :cl-gserver.utils :lparallel :log4cl)
  (:export #:message-box
           #:submit
           #:stop))

(in-package :cl-gserver.messageb)

(defclass message-box ()
  ((name :initform (mkstr "messagebox-" (random 1000000)))
   (handle-message-fun :initarg :handle-message-fun
                       :documentation "The message handler function.")
   (message-kernel :initform nil
                   :documentation
                   "The message-kernel with 1 worker for handling the messages.")
   (message-channel :initform nil
                    :documentation
                    "The message-channel for the message-kernel. Since we only have 1 worker here it is safe to make an instance channel regarding FIFO.")))

(defmethod initialize-instance :after ((self message-box) &key)
  :documentation "Initializes the instance."

  (log:debug "Initialize instance: ~a~%" self)
  
  (with-slots (message-kernel
               message-channel
               name) self
     (let ((*kernel* (make-kernel 1 :name (mkstr "message-kernel-" name))))
       (setf message-kernel *kernel*)
       (setf message-channel (make-channel)))))

(defgeneric submit (message-box gserver message withreply-p)
  (:documentation "Submit a message to the mailbox to be queued and handled."))

(defmethod submit ((self message-box) gserver message withreply-p)
  (let* ((*task-category* (mkstr (slot-value self 'name) "-task"))
         (*kernel* (slot-value self 'message-kernel))
         (channel (slot-value self 'message-channel))
         (msg-handle-fun (slot-value self 'handle-message-fun)))
    (log:debug "Channel: " channel)
    (log:debug "Pushing ~a to channel" message)
    (submit-task channel (lambda ()
                           (funcall msg-handle-fun gserver message withreply-p)))

    (if withreply-p
        (receive-result channel)
        (progn
          (future (receive-result channel))
          t))))

(defun stop (message-box)
  "Stops the message handling and the message handling thread."
  (with-slots (message-kernel) message-box
    (let ((*kernel* message-kernel))
      (end-kernel :wait t))))
