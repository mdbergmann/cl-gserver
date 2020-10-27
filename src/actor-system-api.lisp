(defpackage :cl-gserver.actor-system-api
  (:use :cl)
  (:nicknames :system-api)
  (:export #:dispatchers))

(in-package :cl-gserver.actor-system-api)

(defgeneric dispatchers (system))
