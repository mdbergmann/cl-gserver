(defpackage :cl-gserver.queue
  (:use :cl)
  (:nicknames :queue)
  (:export #:queue-tfifo
           #:queue-bounded
           #:queue-unbounded
           #:pushq
           #:popq))

(in-package :cl-gserver.queue)

#|
A stmx based queue implementation.
Tests showwd that this is quite a bit slower than
the unbounded and bounded queues we use.
|#

(defclass queue-tfifo (queue-base)
    ((queue :initform (stmx.util:tfifo))))
(defmethod pushq ((self queue-tfifo) element)
  (with-slots (queue) self
    (stmx.util:put queue element)))
(defmethod popq ((self queue-tfifo))
  (with-slots (queue) self
    (stmx.util:take queue)))
