(defpackage :cl-gserver.wheel-timer
  (:use :cl)
  (:nicknames :wt)
  (:export #:wheel-timer
           #:make-wheel-timer))

(in-package :cl-gserver.wheel-timer)

(defclass wheel-timer () ())

(defun make-wheel-timer (config)
  (make-instance 'wheel-timer))
