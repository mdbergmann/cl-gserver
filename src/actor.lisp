(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver)
  (:nicknames :act)
  (:export #:actor
           #:receive
           #:send
           #:ask))

(in-package :cl-gserver.actor)

(defclass actor (gserver) ()
  (:documentation
"Specialized `gserver' class called `actor'.
There is a different terminology behind `actor'.
I.e. There is only one `receive' function.
And there is asynchronous `send' and synchronous `ask'.
So there is not much difference to a `gserver'.
It only uses one method `receive'. However both `handle-call' and `handle-cast' og `gserver'
end up in `receive'."))

(defgeneric receive (actor message current-state)
  (:documentation
"The `receive' method handles all messages to an `actor' being it `send' or `ask'.
But the convention persists that the result of `receive' must be a `cons' where
`car' is to be returned to the caller (for `ask') and `cdr' will update the state."))

(defmethod handle-cast ((self actor) message current-state)
  (receive self message current-state))
(defmethod handle-call ((self actor) message current-state)
  (receive self message current-state))

(defun send (actor message)
"Sends a message to the `actor'. `send' is asynchronous."
  (cast actor message))
(defun ask (actor message)
"Sends a message to the `actor'. `ask' is synchronous and waits for a result."
  (call actor message))


;; --------------------
;; Simple actor
;; --------------------
