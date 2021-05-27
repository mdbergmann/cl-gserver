(defpackage :cl-gserver.actor-system
  (:use :cl)
  (:nicknames :asys)
  (:export #:make-actor-system
           #:actor-system
           #:dispatchers
           #:eventstream
           #:*default-config*))

(in-package :cl-gserver.actor-system)

(defvar *default-config*
  '(:dispatchers
    (:shared (:workers 4 :strategy :random))))
