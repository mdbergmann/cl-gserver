(defpackage :sento.wheel-timer
  (:use :cl)
  (:nicknames :wt)
  (:export #:wheel-timer
           #:make-wheel-timer
           #:schedule-once
           #:schedule-recurring
           #:cancel
           #:shutdown-wheel-timer))

(in-package :sento.wheel-timer)

(defclass wheel-timer ()
  ((wheel :initform nil
          :accessor wheel
          :documentation "The wheel timer.")
   (timer-hash :initform (make-hash-table :test 'equal)
               :accessor timer-hash
               :documentation "Hash table of timers. Primarily used to cancel recurring timers."))
  (:documentation "Wheel timer class"))

(defmethod print-object ((obj wheel-timer) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wheel) obj
      (format stream "wheel-timer resolution: ~a, size: ~a"
              (tw:wheel-resolution wheel)
              (length (tw::slots wheel))))))

(defun make-wheel-timer (&rest config)
  "Creates a new `wt:wheel-timer`.

`config` is a parameter for a list of key parameters including:

- `:resolution` the timer time resolution in milliseconds. 100 milliseconds is a good default.
- `:max-size` the number of timer slots this wheel should have.

Note that an `asys:actor-system` includes an instance as `asys:scheduler` that can be used within actors.
But you can also create your own instance."
  (let ((instance (make-instance 'wheel-timer)))
    (setf (wheel instance)
          (tw:make-wheel (getf config :max-size 500)
                         (getf config :resolution 100)))
    (tw:initialize-timer-wheel (wheel instance))
    instance))

(defun schedule-once (wheel-timer delay timer-fun &key (sig nil) (reuse-sig nil))
  "Schedule a function execution once:

- `wheel-timer` is the `wt:wheel-timer` instance.
- `delay` is the number of seconds (float) delay when `timer-fun` should be executed.
- `timer-fun` is a 0-arity function that is executed after `delay`. BEWARE: the function is executed in the timer thread. Make sure that you off-load long running tasks to other threads, or to a custom dispatcher (i.e. `tasks`).
- `sig` is an optional symbol or string that is used to identify the timer and is used for `cancel`.
- `reuse-sig` is a boolean that indicates whether the signature should be cleaned up after the timer has been executed.

Returns: signature (symbol) that represents the timer and can be used to cancel the timer."
  (let ((signature (or sig (gensym "timer-")))
        (timer-hash (timer-hash wheel-timer)))
    (let ((timer (tw:make-timer (lambda (wheel timer)
                                  (declare (ignore wheel timer))
                                  (ignore-errors
                                   (funcall timer-fun))
                                  (unless reuse-sig
                                    (remhash signature timer-hash))))))
      (setf (gethash signature timer-hash) timer)
      (tw:schedule-timer (wheel wheel-timer)
                         timer
                         :milliseconds (round (* delay 1000)))
      signature)))

(defun schedule-recurring (wheel-timer initial-delay delay timer-fun &optional (sig nil))
  "Schedule a recurring function execution:

- `wheel-timer` is the `wt:wheel-timer` instance.
- `initial-delay` is the number of seconds (float) delay when `timer-fun` is executed the first time.
- `delay` is the number of seconds (float) delay when `timer-fun` should be executed.
- `timer-fun` is a 0-arity function that is executed after `delay`. BEWARE: the function is executed in the timer thread. Make sure that you off-load long running tasks to other threads, or to a custom dispatcher (i.e. `tasks`).
- `sig` is an optional symbol or string that is used to identify the timer and is used for `cancel-recurring`.

Returns the signature that was either passed in via `sig` or a generated one.
The signature can be used to cancel the timer via `cancel-recurring`."
  (let ((signature (or sig (gensym "recurring-timer-")))
        (timer-hash (timer-hash wheel-timer))
        (recurring-timer-fun))
    (setf recurring-timer-fun
          (lambda ()
            ;; only if signature still exists in hash-table.
            ;; the timer could have been cancelled.
            (when (gethash signature timer-hash)
              (funcall timer-fun)
              (schedule-once wheel-timer delay recurring-timer-fun :sig signature :reuse-sig t))))
    (schedule-once wheel-timer initial-delay recurring-timer-fun :sig signature :reuse-sig t)
    signature))

(defun cancel (wheel-timer sig)
  "Cancels a timer with the given signature `sig`."
  (let ((timer (gethash sig (timer-hash wheel-timer))))
    (remhash sig (timer-hash wheel-timer)) ;; can be removed anyway
    (when timer
      (tw:uninstall-timer (wheel wheel-timer) timer))))

(defun shutdown-wheel-timer (wheel-timer)
  "Shuts down the wheel timer and free resources."
  (clrhash (timer-hash wheel-timer))
  (tw:shutdown-timer-wheel (wheel wheel-timer)))
