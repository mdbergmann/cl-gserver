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
   (ev-actor :initform nil))
  (:documentation "Eventstream facility allows any actor in the system to publish events,
and actors that did subscribe to listen to events.

Events can be posted as plain strings, where the subscriber has to define the exact string match.

For example: a subscriber wants to listen to events with the string \"Foo\".
The subscriber is then only notified when the events is posted with the exact same string.

Or events can be symbols.
For example: a subscriber wants to listen to a certain type of message which is represented by the symbol `'my-package:foo` (this can be a class type or something else).
The subscriber is then notified about the event when the poster posts exactly: `'my-package:foo`."))

(defun make-eventstream (actor-context)
  "Creating an eventstream is done by the `actor-system` which is available system wide. 
But in theory it can be created individually by just passing an `actor-context` (though I don't know what would be the reason to create an eventstream for the context of a single actor. Maybe to address only a certain hierarchy in the actor tree.)"
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
  "Subscribe to the events stream for a certain message type, or all messages when `message` is nil.
The `message` can be:
- a string: in which case an exact string comparison is made for a string message that is posted to the eventstream.
- a symbol: a symbol can be just `'string` to receive notifications about all messages that are of type `string`.
Or it may be class, or otherwise defined symbol."
  (with-slots (subscribers) self
    (setf subscribers (cons `(,actor ,message) subscribers))))

(defmethod unsubscribe ((self eventstream) (actor act:actor))
  "Unsubscribe from the eventstream. No more events will be received then."
  (with-slots (subscribers) self
    (setf subscribers (remove-if (lambda (x) (eq x actor)) subscribers :key #'car))))

(defmethod publish ((self eventstream) message)
  "Publish an event/message to the eventstream. Subscribers may receive notification if they registered for the right message type."
  (with-slots (ev-actor) self
    (tell ev-actor message)))
