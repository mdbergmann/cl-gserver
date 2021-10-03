(defpackage :cl-gserver.atomic
  (:use :cl)
  (:nicknames :atomic)
  (:export #:make-atomic-reference
           #:make-atomic-integer
           #:atomic-cas
           #:atomic-get))

(defgeneric atomic-get (atomic)
  (:documentation "Retrieve value from atomic object."))

(defgeneric atomic-cas (atomic expected new)
  (:documentation "Set `new` value. The current value must be `extented`."))
