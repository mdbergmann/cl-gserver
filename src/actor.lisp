(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver.actor-cell)
  (:nicknames :act)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:actor
           #:tell
           #:ask
           #:async-ask
           #:after-start
           #:cell
           #:system
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
(defgeneric system (actor)
  (:documentation "Access to the `actor-system'."))

(defclass actor (actor-api)
  ((cell :initform nil
         :reader cell
         :documentation "The inner `actor-cell'.")
   (receive-fun :initarg :receive-fun
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

(defmethod initialize-instance :after ((self actor) &key name state msgbox)
  (with-slots (cell) self
    (setf cell (make-instance 'actor-cell
                              :name name
                              :state state
                              :msgbox msgbox)))
  (log:debug "After initialize: ~a" self))

(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (cell) obj
      (format stream "cell: ~a" cell))))

(defmethod handle-call ((self actor) message state)
  (funcall (receive-fun self) message state))
(defmethod handle-cast ((self actor) message state)
  (funcall (receive-fun self) message state))

(defmethod system ((self actor))
  (act-cell:system (cell self)))

(defmethod tell ((self actor) message)
  (cast (cell self) message))
(defmethod ask ((self actor) message)
  (call (cell self) message))

(defmethod after-start ((self actor) state)
  (with-slots (after-start-fun) self
    (when after-start-fun
      (funcall after-start-fun self state))))

;; (defmethod async-ask ((self actor) message)
;;   (make-future (lambda (promise-fun)
;;                  (log:debug "Executing fcomputation function...")
;;                  (make-system-actor (system self)
;;                                     (%make-waitor-actor (cell self) message
;;                                                         (lambda (result)
;;                                                           (log:debug "Result: ~a~%" result)
;;                                                           (funcall promise-fun result)))))))

;; (defmacro %make-waitor-actor (actor message &rest body)
;;   (with-gensyms (self msg state)
;;     `(lambda ()
;;        (make-instance 'actor 
;;                       :receive-fun (lambda (,self ,msg ,state)
;;                                      (unwind-protect
;;                                           (progn
;;                                             (funcall ,@body ,msg)
;;                                             (tell ,self :stop)
;;                                             (cons ,msg ,state))
;;                                        (tell ,self :stop)))
;;                       :after-start-fun (lambda (,self ,state)
;;                                          (declare (ignore ,state))
;;                                          ;; this will call the `tell' function
;;                                          (cell::submit-message ,actor ,message nil ,self))
;;                       :name (string (gensym "Async-ask-waiter-"))))))
