(defpackage :cl-gserver.wheel-timer
  (:use :cl)
  (:nicknames :wt)
  (:export #:wheel-timer
           #:make-wheel-timer
           #:schedule
           #:shutdown-wheel-timer))

(in-package :cl-gserver.wheel-timer)

(defclass wheel-timer ()
  ((wheel :initform nil
          :accessor wheel
          :documentation "The wheel.")))

(defun make-wheel-timer (config)
  (let ((instance (make-instance 'wheel-timer)))
    (setf (wheel instance)
          (tw:make-wheel (config:retrieve-value config :max-size)
                         (config:retrieve-value config :resolution)))
    (tw:initialize-timer-wheel (wheel instance))
    instance))

(defun schedule (wheel-timer millis timer-fun)
  (tw:schedule (wheel wheel-timer)
                     (tw:make-timer (lambda (wheel timer)
                                      (declare (ignore wheel timer))
                                      (funcall timer-fun)))
                     :milliseconds millis))

(defun shutdown-wheel-timer (wheel-timer)
  (tw:shutdown-timer-wheel (wheel wheel-timer)))
