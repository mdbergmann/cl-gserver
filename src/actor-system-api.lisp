(defpackage :cl-gserver.actor-system
  (:use :cl)
  (:nicknames :asys)
  (:export #:make-actor-system
           #:actor-system
           #:dispatchers
           #:*default-config*))

(in-package :cl-gserver.actor-system)

(defparameter *default-config*
  '(:dispatchers (:num-shared-workers 4)))

(defconstant +config-sections+
  '(:dispatchers
    (:keys :num-shared-workers)))
