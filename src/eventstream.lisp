(defpackage :cl-gserver.eventstream
  (:use :cl)
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
   (ev-actor :initform nil))
  (:documentation "Eventstream facility allows any actor in the system to publish events,
and actors that did subscribe to listen to events.

Events can be posted as plain strings, where the subscriber has to define the exact string match.

For example: a subscriber wants to listen to events with the string \"Foo\".
The subscriber is then only notified when the events is posted with the exact same string.

Or events can be symbols.
For example: a subscriber wants to listen to a certain type of message which is represented by the symbol `'my-package:foo` (this can be a class type or something else).
The subscriber is then notified about the event when the poster posts exactly: `'my-package:foo`.

See more information at the `subscribe` function."))

(defun make-eventstream (actor-context)
  "Creating an eventstream is done by the `actor-system` which is available system wide. 
But in theory it can be created individually by just passing an `actor-context` (though I don't know what would be the reason to create an eventstream for the context of a single actor. Maybe to address only a certain hierarchy in the actor tree.)"
  (let ((ev (make-instance 'eventstream)))
    (with-slots (ev-actor) ev
      (setf ev-actor (actor-of (actor-context
                                (gensym "eventstream-actor-")
                                :dispatcher :pinned)
                       (lambda (ev-stream msg state)
                         (handler-case
                             (ev-receive ev ev-stream msg state)
                           (t (c)
                             (log:warn "Condition: ~a" c)
                             (cons t state)))))))
    ev))

(defun ev-receive (ev listener msg state)
  (declare (ignore listener))
  (with-slots (subscribers) ev
    (let* ((msg-type (type-of msg))
           (subs (subscribers-for subscribers msg-type msg)))
      (dolist (sub subs)
        (tell sub msg))))
  (cons t state))

(defun subscribers-for (subscribers msg-type msg)
  ;;(format t "msg-type, msg: ~a, ~a~%" msg-type msg)
  (flet ((no-type-registered-p (elem) (null elem))
         (equal-string-p (elem) (and (stringp msg)
                                     (typep elem 'string)
                                     (string= elem msg)))
         (equal-list-p (elem) (and (listp msg)
                                   (typep elem 'cons)
                                   (equalp elem msg)))
         (equal-symbol-p (elem) (and (symbolp msg)
                                     (symbolp elem)
                                     (eq elem msg)))
         (equal-objecttype-p (elem) (and (symbolp elem)
                                         (not (symbolp msg))
                                         (subtypep elem msg-type))))
    (mapcar #'car
            (filter (lambda (sub)
                      (let ((reg-type (second sub)))
                        (or (no-type-registered-p reg-type)
                            (or (equal-symbol-p reg-type)
                                (equal-objecttype-p reg-type)
                                (equal-string-p reg-type)
                                (equal-list-p reg-type)))))
                    subscribers))))

(defgeneric subscribe (eventstream subscriber &optional pattern)
  (:documentation
   "Subscribe to the eventstream to receive notifications of certain events or event types.

The `pattern` can be:
- nil: receive all events posted to the eventstream.
- a type, class type: this allows to get notifications when an instance of this type, or class type is posted.
- a symbol or global symbol: if posted message is a symbol or global symbol then the symbols are compared (`eq`).
- a string: in which case an exact string comparison is made for a string message that is posted to the eventstream.
- a list: if subscription if for a list structure, and the posted message is also a list structure, then a structure comparison (`equalp`) is made."))

(defmethod subscribe ((ev-stream eventstream) (subscriber act:actor) &optional pattern)
  (with-slots (subscribers) ev-stream
    (setf subscribers (cons `(,subscriber ,pattern) subscribers))))

(defgeneric unsubscribe (eventstream unsubscriber)
  (:documentation
   "Unsubscribe from the eventstream. No more events will be received then."))

(defmethod unsubscribe ((ev-stream eventstream) (unsubscriber act:actor))
  (with-slots (subscribers) ev-stream
    (setf subscribers (remove-if (lambda (x) (eq x unsubscriber)) subscribers :key #'car)))
  t)

(defgeneric publish (eventstream message)
  (:documentation
   "Publish an event/message to the eventstream. Subscribers may receive notification if they registered for the right message pattern."))

(defmethod publish ((ev-stream eventstream) message)  
  (with-slots (ev-actor) ev-stream
    (tell ev-actor message)))
