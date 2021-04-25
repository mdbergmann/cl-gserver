(defpackage :cl-gserver.eventstream
  (:use :cl)
  (:nicknames :ev)
  (:import-from #:utils
                #:filter)
  (:import-from #:act
                #:actor-of
                #:tell)
  (:export #:eventstream
           #:make-eventstream
           #:subscribe
           #:unsubscribe
           #:publish))

(in-package :cl-gserver.eventstream)

(defgeneric subscribe (eventstream subscriber &optional pattern)
  (:documentation
   "Subscribe to the eventstream to receive notifications of certain events or event types.

`subscriber` must be an actor (or agent).

The `pattern` can be:

- nil: receive all events posted to the eventstream.
- a type, class type: this allows to get notifications when an instance of this type, or class type is posted.
I.e. if you want to listen to all string messages posted to the ev, thewn subscribe to `'string`.
Or if you want to listen to all lists, subscribe with `'cons`.
- a symbol or global symbol: if posted message is a symbol or global symbol then the symbols are compared (`eq`).
- a string: in which case an exact string comparison is made for a string message that is posted to the eventstream (`string=`).
- a list: if subscription if for a list structure, and the posted message is also a list structure, then a structure comparison (`equalp`) is made."))

(defgeneric unsubscribe (eventstream unsubscriber)
  (:documentation
   "Unsubscribe from the eventstream. No more events will be received then."))

(defgeneric publish (eventstream message)
  (:documentation
   "Publish an event/message to the eventstream. Subscribers may receive notification if they registered for the right message pattern."))
