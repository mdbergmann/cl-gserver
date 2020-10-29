(defpackage :cl-gserver.dispatcher
  (:use :cl)
  (:nicknames :disp)
  (:export #:shutdown
           #:dispatch
           #:dispatch-async
           ))

(in-package :cl-gserver.dispatcher)

(defgeneric shutdown (dispatcher))
(defgeneric dispatch (dispatcher dispatcher-exec-fun))
(defgeneric dispatch-async (dispatcher dispatcher-exec-fun))
