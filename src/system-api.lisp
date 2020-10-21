(defpackage :cl-gserver.system-api
  (:use :cl)
  (:nicknames :system-api)
  (:export #:shutdown
           #:get-dispatcher))

(in-package :cl-gserver.system-api)

(defgeneric shutdown (system))
(defgeneric get-dispatcher (system))
