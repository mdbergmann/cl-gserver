(defpackage :cl-gserver.system-actor
  (:use :cl :cl-gserver.actor :cl-gserver.system-api)
  (:nicknames :system-actor)
  (:import-from #:cl-gserver.future
                #:make-future)
  (:import-from #:gs
                #:msgbox)
  (:import-from #:mesgb
                #:message-box-dp)
  (:export #:system-actor
           #:make-system-actor
           #:get-system
           #:system-actor-p))

(in-package :cl-gserver.system-actor)

;; --------------------------------------
;; system actor
;; --------------------------------------

(defclass system-actor (wrapping-actor-base)
  ((system :initarg :system
           :initform nil
           :type 'system
           :reader get-system
           :documentation "The system of this actor."))
  (:documentation
   "A 'system' actor is instanciated as part of a `system'.
The message dispatch in this case works using a shared thread pool."))

(defun system-actor-p (obj)
  (typep obj 'system-actor))

(defmethod print-object ((obj system-actor) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (system) obj
      (format stream "system: ~a" system))))

(defmethod async-ask ((self system-actor) message)
  (make-future (lambda (promise-fun)
                 (log:debug "Executing fcomputation function...")
                 (make-system-actor (get-system self)
                                    (%make-waitor-actor (the-wrapped self) message
                                                        (lambda (result)
                                                          (log:debug "Result: ~a~%" result)
                                                          (funcall promise-fun result)))))))

(defun make-system-actor (system creator-fun)
  (let ((inner-actor (funcall creator-fun))
        (system-actor (make-instance 'system-actor :system system)))

    (unless (typep inner-actor 'actor)
      (error "actor-class is no subclass of 'actor"))

    (setf (the-wrapped system-actor) inner-actor)
      
    (with-slots (msgbox) (the-wrapped system-actor)
      (setf msgbox (make-instance 'message-box-dp
                                  :dispatcher (get-dispatcher system))))
    system-actor))
