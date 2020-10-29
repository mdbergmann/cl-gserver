(defpackage :cl-gserver.actor-system
  (:use :cl)
  (:nicknames :asys)
  (:export #:dispatchers))

(in-package :cl-gserver.actor-system)

(defgeneric dispatchers (system))
