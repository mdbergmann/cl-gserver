(defpackage :cl-gserver.dispatcher-api
  (:use :cl)
  (:nicknames :dispatcher-api)
  (:export #:shutdown
           #:dispatch
           #:dispatch-async
           ))

(in-package :cl-gserver.dispatcher-api)

(defgeneric shutdown (dispatcher))
(defgeneric dispatch (dispatcher fun))
(defgeneric dispatch-async (dispatcher fun))
