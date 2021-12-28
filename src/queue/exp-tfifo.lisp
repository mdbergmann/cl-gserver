(defpackage :cl-gserver.queue
  (:use :cl)
  (:nicknames :queue)
  (:export #:queue-tfifo
           #:queue-bounded
           #:queue-unbounded
           #:pushq
           #:popq))

(in-package :cl-gserver.queue)

(defclass queue-tfifo (queue-base)
    ((queue :initform (stmx.util:tfifo))))
(defmethod pushq ((self queue-tfifo) element)
  (with-slots (queue) self
    (stmx.util:put queue element)))
(defmethod popq ((self queue-tfifo))
  (with-slots (queue) self
    (stmx.util:take queue)))
