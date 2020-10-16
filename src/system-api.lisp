(defpackage :cl-gserver.system-api
  (:use :cl)
  (:nicknames :system-api)
  (:export #:shutdown
           #:dispatcher))

(in-package :cl-gserver.system-api)

(defgeneric shutdown (system))
(defgeneric dispatcher (system))
