(defpackage :cl-gserver.actor-system
  (:use :cl)
  (:nicknames :asys)
  (:export #:make-actor-system
           #:actor-system
           #:dispatchers
           #:eventstream
           #:timeout-timer
           #:register-dispatcher
           #:register-new-dispatcher
           #:*default-config*))

(in-package :cl-gserver.actor-system)

(defparameter *default-config*
  '(:dispatchers
    (:shared (:workers 4 :strategy :random))
    :timeout-timer
    (:resolution 500 :max-size 1000)
    :eventstream
    (:dispatcher-id :shared))
  "The default config used when creating an `asys:actor-system`.
The actor-system constructor allows to provide custom config options that override the default.")
