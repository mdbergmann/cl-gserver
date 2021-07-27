(defpackage :cl-gserver.agent.usecase-commons
  (:use :cl)
  (:nicknames :agent.usecase-commons)
  (:export #:model
           #:make-model
           #:model-object
           #:model-err-fun
           #:with-update-handler
           #:with-get-handler))

(in-package :cl-gserver.agent.usecase-commons)

(defstruct model
  (object nil)
  (err-fun nil))

(defmacro with-update-handler (&body body)
  `(lambda (model)
     (handler-case
         ,@body
       (error (c)
         (when (model-err-fun model)
           (funcall (model-err-fun model) c))))
     model))

(defmacro with-get-handler (&body body)
  `(lambda (model)
    (handler-case
        ,@body
      (error (c) c))))

