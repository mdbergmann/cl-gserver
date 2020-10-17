(defpackage :cl-gserver.system-api
  (:use :cl)
  (:nicknames :system-api)
  (:export #:shutdown
           #:dispatcher
           #:actor-creator
           #:actor-of
           #:actors))

(in-package :cl-gserver.system-api)

(defgeneric shutdown (system))
(defgeneric dispatcher (system))

;; ---------------------------------
;; Actor creator
;; ---------------------------------

(defclass actor-creator ()
  ((actors :initform '()
           :reader actors
           :documentation "A list of actors.")))

(defgeneric actor-of (actor-creator create-fun))
