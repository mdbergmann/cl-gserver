(defpackage :sento.remoting
  (:use :cl)
  (:nicknames :rem)
  (:export #:enable-remoting
           #:disable-remoting
           #:make-remote-ref
           ;; conditions
           #:remoting-error
           #:remoting-error-message))

(in-package :sento.remoting)

;; ---------------------------------
;; base condition
;; ---------------------------------

(define-condition remoting-error (error)
  ((message :initarg :message
            :reader remoting-error-message
            :initform ""
            :documentation "Human-readable error description."))
  (:report (lambda (c stream)
             (format stream "Remoting error: ~a" (remoting-error-message c))))
  (:documentation "Base condition for all remoting errors."))
