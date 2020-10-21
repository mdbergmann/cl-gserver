(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver.gserver)
  (:nicknames :act)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:actor
           #:tell
           #:ask
           #:async-ask
           #:after-start
           ;; wrapping-actor
           #:wrapping-actor-base
           #:the-wrapped
           #:%make-waitor-actor)
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

;; -------------------------------------
;; Wrapping actor
;; -------------------------------------

(defclass wrapping-actor-base ()
  ((wrapped-actor :initform nil
                  :accessor the-wrapped
                  :documentation "The wrapped actor. `wrapping-actor-base' acts as a facade."))
  (:documentation
   "This actor wraps another actor. This acts as a base class for `single-actor' and `system-actor'"))

(defmethod print-object ((obj wrapping-actor-base) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wrapped-actor) obj
      (format stream "wrapped: ~a" wrapped-actor))))

(defmethod after-start ((self wrapping-actor-base) state)
  (after-start (the-wrapped self) state))

(defmethod tell ((self wrapping-actor-base) message)
  (tell (the-wrapped self) message))

(defmethod ask ((self wrapping-actor-base) message)
  (ask (the-wrapped self) message))

(defmacro %make-waitor-actor (actor message &rest body)
  (with-gensyms (self msg state)
    `(lambda ()
       (make-instance 'actor 
                      :receive-fun (lambda (,self ,msg ,state)
                                     (unwind-protect
                                          (progn
                                            (funcall ,@body ,msg)
                                            (tell ,self :stop)
                                            (cons ,msg ,state))
                                       (tell ,self :stop)))
                      :after-start-fun (lambda (,self ,state)
                                         (declare (ignore ,state))
                                         ;; this will call the `tell' function
                                         (gs::submit-message ,actor ,message nil ,self))
                      :name (string (gensym "Async-ask-waiter-"))))))
