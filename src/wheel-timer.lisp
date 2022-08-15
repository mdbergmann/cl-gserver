(defpackage :sento.wheel-timer
  (:use :cl)
  (:nicknames :wt)
  (:export #:wheel-timer
           #:make-wheel-timer
           #:schedule
           #:shutdown-wheel-timer))

(in-package :sento.wheel-timer)

(defclass wheel-timer ()
  ((wheel :initform nil
          :accessor wheel
          :documentation "The wheel."))
  (:documentation "Wheel timer class"))

(defmethod print-object ((obj wheel-timer) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wheel) obj
      (format stream "wheel resolution: ~a, size: ~a"
              (tw:wheel-resolution wheel)
              (length (tw::slots wheel))))))

(defun make-wheel-timer (&rest config)
  "Creates a new `wheel-timer`.

`config` is a parameter for a list of key parameters including:
  `:resolution`: the timer time resolution in milliseconds.  
  `:max-size`: the maximum size of timer functions this wheel can handle."
  (let ((instance (make-instance 'wheel-timer)))
    (setf (wheel instance)
          (tw:make-wheel (getf config :max-size)
                         (getf config :resolution)))
    (tw:initialize-timer-wheel (wheel instance))
    instance))

(defun schedule (wheel-timer delay timer-fun)
  "Schedule a function execution:

`wheel-timer` is the `wt:wheel-timer` instance.
`delay` is the number of seconds (float) delay when `timer-fun` should be executed.
`timer-fun` is a 0-arity function that is executed after `delay`."
  (tw:schedule-timer (wheel wheel-timer)
                     (tw:make-timer (lambda (wheel timer)
                                      (declare (ignore wheel timer))
                                      (funcall timer-fun)))
                     :milliseconds (round (* delay 1000))))

(defun shutdown-wheel-timer (wheel-timer)
  "Shuts down the wheel timer and frees resources."
  (tw:shutdown-timer-wheel (wheel wheel-timer)))
