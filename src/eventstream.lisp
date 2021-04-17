(defpackage :cl-gserver.eventstream
  (:use :cl :arrows)
  (:nicknames :ev)
  (:import-from #:act
                #:actor-of
                #:tell)
  (:import-from #:utils
                #:filter)
  (:export #:make-eventstream
           #:subscribe
           #:unsubscribe
           #:publish))

(in-package :cl-gserver.eventstream)

(defclass eventstream ()
  ((subscribers :initform '()
                :reader subscribers)
   (ev-actor :initform nil)))

(defun make-eventstream (actor-context)
  (let ((ev (make-instance 'eventstream)))
    (with-slots (ev-actor) ev
      (setf ev-actor (actor-of (actor-context
                                (gensym "eventstream-actor")
                                :dispatcher :pinned)
                       (lambda (self msg state)
                         (handler-case
                             (ev-receive ev self msg state)
                           (t (c)
                             (log:warn "Condition: ~a" c)
                             (cons t state)))))))
    ev))

(defun ev-receive (ev listener msg state)
  (declare (ignore listener))
  (with-slots (subscribers) ev
    (let* ((msg-type (type-of msg))
           (subs (subscribers-for-type subscribers msg-type msg)))
      (dolist (sub subs)
        (tell sub msg))))
  (cons t state))

(defun subscribers-for-type (subscribers msg-type msg)
  (flet ((is-no-type-registered (elem) (null elem))
         (is-equal-subtype (elem) (subtypep (type-of elem) msg-type))
         (is-equal-string (elem) (and (subtypep msg-type 'string)
                                      (string= elem msg)))
         (is-equal-symbol (elem) (and (symbolp msg-type)
                                      (eq elem msg))))
    (mapcar #'car
            (filter (lambda (sub)
                      (or (is-no-type-registered (second sub))
                          (and (is-equal-subtype (second sub))
                               (or (is-equal-string (second sub))
                                   (is-equal-symbol (second sub))))))
                    subscribers))))

(defmethod subscribe ((self eventstream) (actor act:actor) &optional message)
  (with-slots (subscribers) self
    (setf subscribers (cons `(,actor ,message) subscribers))))

(defmethod unsubscribe ((self eventstream) (actor act:actor))
  (with-slots (subscribers) self
    (setf subscribers (remove-if (lambda (x) (eq x actor)) subscribers :key #'car))))

(defmethod publish ((self eventstream) message)
  (with-slots (ev-actor) self
    (tell ev-actor message)))
