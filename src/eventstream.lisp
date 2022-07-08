
(in-package :cl-gserver.eventstream)

(shadowing-import '(ev:subscribe
                    ev:unsubscribe
                    ev:publish))

(defclass eventstream ()
  ((subscribers :initform '()
                :reader subscribers)
   (ev-actor :initform nil))
  (:documentation "Eventstream facility allows to post/publish messages/events in the `asys:actor-system` and actors that did subscribe, to listen on those events.

The eventstream is driven by an actor. The processing of the sent events is guaranteed to be as they arrive.

Events can be posted as plain strings, as lists, or as objects of classes.
The subscriber has a variaty of options to define what to listen for.

For example: a subscriber wants to listen to events/messages with the string \"Foo\".
The subscriber is then only notified when events are posted with the exact same string.

See more information at the `ev:subscribe` function."))

(defun make-eventstream (actor-context &rest config)
  "Creating an eventstream is done by the `asys:actor-system` which is then available system wide. 
But in theory it can be created individually by just passing an `ac:actor-context` (though I don't know what would be the reason to create an eventstream for the context of a single actor. Maybe to address only a certain hierarchy in the actor tree.)

- `actor-context`: the `ac:actor-context` where the eventstream actor should be created in.
- `config`: is a plist with the `:dispatcher-id` key and a dispatcher id as value. Defaults to `:shared`. This dispatcher type should be used by the actor."
  (let ((ev (make-instance 'eventstream)))
    (with-slots (ev-actor) ev
      (setf ev-actor (ac:actor-of actor-context
                       :name (gensym "eventstream-actor-")
                       :dispatcher (getf config :dispatcher-id :shared)
                       :receive (lambda (ev-stream msg state)
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
                                         (subtypep msg-type elem))))
    (mapcar #'car
            (filter (lambda (sub)
                            (let ((reg-type (second sub)))
                              (or (no-type-registered-p reg-type)
                                  (or (equal-symbol-p reg-type)
                                      (equal-objecttype-p reg-type)
                                      (equal-string-p reg-type)
                                      (equal-list-p reg-type)))))
                    subscribers))))

(defmethod subscribe ((ev-stream eventstream) (subscriber act:actor) &optional pattern)
  "Subscribe to `ev:eventstream`."
  (with-slots (subscribers) ev-stream
    (push `(,subscriber ,pattern) subscribers)))

(defmethod unsubscribe ((ev-stream eventstream) (unsubscriber act:actor))
  "Unsubscribe to `ev:eventstream`."
  (with-slots (subscribers) ev-stream
    (setf subscribers (remove-if (lambda (x) (eq x unsubscriber)) subscribers :key #'car)))
  t)

(defmethod publish ((ev-stream eventstream) message)  
  "Publish to `ev:eventstream`."
  (with-slots (ev-actor) ev-stream
    (tell ev-actor message)))
