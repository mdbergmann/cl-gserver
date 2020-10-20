(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver.gserver)
  (:nicknames :act)
  (:export #:actor
           #:tell
           #:ask
           #:async-ask
           #:after-start)
  ;;#:with-actor)
  )

(in-package :cl-gserver.actor)

;; -------------------------------------------------------
;; Actor API
;; -------------------------------------------------------
(defclass actor-api () ()
  (:documentation "This represents the API of an actor."))
(defgeneric tell (actor message)
  (:documentation
   "Sends a message to the `actor'. `tell' is asynchronous. There is no result."))
(defgeneric ask (actor message)
  (:documentation
  "Sends a message to the `actor'. `ask' is synchronous and waits for a result."))
(defgeneric async-ask (actor message)
  (:documentation
   "This returns a `future'.
An `async-ask' is similar to a `ask' in that the caller gets back a result 
but it doesn't have to actively wait for it. Instead a `future' wraps the result.
However, the internal message handling is based on `tell'.
How this works is that the message to the target `actor' is not 'sent' using the callers thread
but instead an anonymous `actor' is started behind the scenes and this in fact makes tells
the message to the target `actor'. It does sent itself along as 'sender'.
The target `actor' tells a response back to the initial `sender'. When that happens and the anonymous `actor'
received the response the `future' will be fulfilled with the `promise'."))
(defgeneric after-start (actor state)
  (:documentation
   "Generic function definition that you may call from `initialize-instance'."))

(defclass actor (gserver)
  ((receive-fun :initarg :receive-fun
                :initform (error "'receive-fun' must be specified!")
                :reader receive-fun)
   (after-start-fun :initarg :after-start-fun
                    :initform nil
                    :reader after-start-fun
                    :documentation "Code to be called after actor start."))
  (:documentation
   "Specialized `gserver' class called `actor'.
There is a different terminology behind `actor'.
I.e. There is only one `receive' function.
And there is asynchronous `tell' and synchronous `ask'.
So there is not much difference to a `gserver'.
It only uses one method `receive'. However both `handle-call' and `handle-cast' of `gserver'
end up in `receive'.
To stop an actors message processing in order to cleanup resouces you should tell (either `tell' or `ask')
the `:stop' message. It will respond with `:stopped'."))

(defmethod handle-cast ((self actor) message current-state)
  (funcall (receive-fun self) self message current-state))
(defmethod handle-call ((self actor) message current-state)
  (funcall (receive-fun self) self message current-state))

(defmethod after-start ((self actor) state)
  (with-slots (after-start-fun) self
    (when after-start-fun
      (funcall after-start-fun self state))))

(defmethod tell ((self actor) message)
  (cast self message))
(defmethod ask ((self actor) message)
  (call self message))
