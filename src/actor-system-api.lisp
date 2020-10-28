(defpackage :cl-gserver.actor-system
  (:use :cl)
  (:nicknames :sys)
  (:export #:dispatchers))

(in-package :cl-gserver.actor-system)

(defgeneric dispatchers (system))
