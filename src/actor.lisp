(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver)
  (:nicknames :act)
  (:export #:actor
           #:receive
           #:send
           #:ask
           #:make-actor))

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
(defclass simple-actor (actor)
  ((receive-fun :initarg :receive-fun
                :initform nil
                :documentation "The receive function as specified as slot."))
  (:documentation
   "A simplified actor that can be created with just `make-actor'."))

(defmethod receive ((self simple-actor) message current-state)
  (with-slots (receive-fun) self
    (when receive-fun
      (funcall receive-fun self message current-state))))


(defun make-actor (name &key state receive-fun)
  "Makes a new `simple-actor' which allows you to specify a name with `:state' and `:receive-fun'."
  (make-instance 'simple-actor :name name
                               :state state
                               :receive-fun receive-fun))
