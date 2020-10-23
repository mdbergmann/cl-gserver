(defpackage :cl-gserver.actor-system-api
  (:use :cl)
  (:nicknames :system-api)
  (:export #:shutdown
           #:message-dispatcher))

(in-package :cl-gserver.actor-system-api)

(defgeneric shutdown (system))
(defgeneric message-dispatcher (system))
