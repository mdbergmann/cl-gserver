(defpackage :cl-gserver.system-api
  (:use :cl)
  (:nicknames :system-api)
  (:export #:shutdown
           #:dispatcher))

(in-package :cl-gserver.system-api)

(defgeneric shutdown (system))
(defgeneric dispatcher (system))

;; ---------------------------------
;; Containers
;; ---------------------------------

(in-package :cl-user)
(defpackage :cl-gserver.actor-container
  (:use :cl)
  (:nicknames :actor-container)
  (:export #:actor-container
           #:actor-of
           #:get-actors
           #:add-actor))
(in-package :cl-gserver.actor-container)

(defclass actor-container ()
  ((actors :initform '()
           :reader get-actors
           :documentation "A list of actors.")))

(defgeneric actor-of (actor-container create-fun))

(defgeneric add-actor (actor-container actor))

(defmethod add-actor ((self actor-container) actor)
  (with-slots (actors) self
    (setf actors (list* actor actors))))
