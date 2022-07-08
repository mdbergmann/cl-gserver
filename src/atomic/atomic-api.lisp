(defpackage :cl-gserver.atomic
  (:use :cl)
  (:nicknames :atomic)
  (:export #:make-atomic-reference
           #:make-atomic-integer
           #:atomic-get
           #:atomic-swap))

(in-package :cl-gserver.atomic)

(defgeneric atomic-get (atomic)
  (:documentation "Retrieve value from atomic object."))

(defgeneric atomic-cas (atomic expected new)
  (:documentation "Set `new` value. The current value must be `extented`."))

(defgeneric atomic-swap (atomic fn &rest args)
  (:documentation "Update the the atomic object to the value returned by calling function `fn` with the previous value of the atomic object and `args`."))
