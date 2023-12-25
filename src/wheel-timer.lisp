(defpackage :sento.wheel-timer
  (:use :cl)
  (:nicknames :wt)
  (:export #:wheel-timer
           #:make-wheel-timer
           #:schedule
           #:schedule-recurring
           #:cancel
           #:cancel-for-sig
           #:shutdown-wheel-timer))

(in-package :sento.wheel-timer)

(defclass wheel-timer ()
  ((wheel :initform nil
          :accessor wheel
          :documentation "The wheel timer.")
   (timer-hash :initform (make-hash-table :test 'equal)
               :accessor timer-hash
               :documentation "Hash table of timers. Used to cancel recurring timers."))
  (:documentation "Wheel timer class"))

(defmethod print-object ((obj wheel-timer) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wheel) obj
      (format stream "wheel-timer resolution: ~a, size: ~a"
              (tw:wheel-resolution wheel)
              (length (tw::slots wheel))))))

(defun make-wheel-timer (&rest config)
  "Creates a new `wheel-timer`.

`config` is a parameter for a list of key parameters including:
  `:resolution`: the timer time resolution in milliseconds.  
  `:max-size`: the maximum size of timer functions this wheel can handle."
  (let ((instance (make-instance 'wheel-timer)))
    (setf (wheel instance)
          (tw:make-wheel (getf config :max-size 500)
                         (getf config :resolution 100)))
    (tw:initialize-timer-wheel (wheel instance))
    instance))

(defun schedule (wheel-timer delay timer-fun)
  "Schedule a function execution once:

`wheel-timer` is the `wt:wheel-timer` instance.
`delay` is the number of seconds (float) delay when `timer-fun` should be executed.
`timer-fun` is a 0-arity function that is executed after `delay`.

returns: a timer object that can be used to cancel the timer.

It is up to the caller to keep a reference to the timer object in case it needs to be cancelled."
  (let ((timer (tw:make-timer (lambda (wheel timer)
                                (declare (ignore wheel timer))
                                (ignore-errors
                                 (funcall timer-fun))))))
    (tw:schedule-timer (wheel wheel-timer)
                       timer
                       :milliseconds (round (* delay 1000)))
    timer))

(defun schedule-recurring (wheel-timer initial-delay delay timer-fun &optional (sig nil))
  "Schedule a recurring function execution:

`wheel-timer` is the `wt:wheel-timer` instance.
`initial-delay` is the number of seconds (float) delay when `timer-fun` is executed the first time.
`delay` is the number of seconds (float) delay when `timer-fun` should be executed.
`timer-fun` is a 0-arity function that is executed after `delay`.
`sig` is an optional symbol or string that is used to identify the timer and is used for `cancel-for-sig`.

returns the signature that was either passed in via `sig` or a generated one.
The signature can be used to cancel the timer via `cancel-for-sig`."
  (let ((signature (or sig (gensym "recurring-timer-")))
        (timer-hash (timer-hash wheel-timer))
        (recurring-timer-fun))
    (setf recurring-timer-fun
          (lambda ()
            ;; only if signature still exists in hash-table
            (when (gethash signature timer-hash)
              (funcall timer-fun)
              (setf (gethash signature timer-hash)
                    (schedule wheel-timer delay recurring-timer-fun)))))
    (setf (gethash signature timer-hash)
          (schedule wheel-timer initial-delay recurring-timer-fun))
    signature))

(defun cancel (wheel-timer timer)
  "Cancels a timer.
`wheel-timer` is the `wt:wheel-timer` instance.
`timer` is the timer object returned by `wt:schedule`."
  (tw:uninstall-timer (wheel wheel-timer) timer))

(defun cancel-for-sig (wheel-timer sig)
  "Cancels a recurring timer with the givenm signature `sig`."
  (cancel wheel-timer (gethash sig (timer-hash wheel-timer)))
  (remhash sig (timer-hash wheel-timer)))

(defun shutdown-wheel-timer (wheel-timer)
  "Shuts down the wheel timer and frees resources."
  (tw:shutdown-timer-wheel (wheel wheel-timer)))
