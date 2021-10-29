(defpackage :cl-gserver.actor-system
  (:use :cl)
  (:nicknames :asys)
  (:export #:make-actor-system
           #:actor-system
           #:dispatchers
           #:eventstream
           #:timeout-timer
           #:*default-config*))

(in-package :cl-gserver.actor-system)

(defparameter *default-config*
  '(:dispatchers
    (:shared (:workers 4 :strategy :random))
    :timeout-timer
    (:resolution 500 :max-size 1000)))
