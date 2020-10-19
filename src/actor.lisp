(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver.gserver :cl-gserver.future)
  (:import-from #:alexandria
                #:with-gensyms)
  (:nicknames :act)
  (:local-nicknames (:mb :cl-gserver.messageb))
  (:export #:actor
           #:single-actor
           #:tell
           #:ask
           #:async-ask
           #:after-init
           #:make-actor
           #:make-single-actor)
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
  "Sends a message to `actor' and waits for a result but asynchronously.
The result is an `fcomputation' which accepts `on-complete' handlers, etc."))
(defgeneric after-init (actor state)
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

(defmethod after-init ((self actor) state)
  (with-slots (after-start-fun) self
    (when after-start-fun
      (funcall after-start-fun self state))))

(defmethod tell ((self actor) message)
  (cast self message))
(defmethod ask ((self actor) message)
  (call self message))

;; -------------------------------------------------------
;; 'single' actor
;; -------------------------------------------------------

(defclass single-actor ()
  ((wrapped-actor :initform nil
                  :reader wrapped-actor
                  :documentation "The wrapped actor. `actor' acts as a facade."))
  (:documentation "A 'single' actor can be instantiated separate of a `system'.
It will run it's own threaded message-box."))

(defmethod print-object ((obj single-actor) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wrapped-actor) obj
      (format stream "wrapped: ~a" wrapped-actor))))

(defmethod after-init ((self single-actor) state)
  (after-init (wrapped-actor self) state))

(defmethod tell ((self single-actor) message)
  (tell (wrapped-actor self) message))

(defmethod ask ((self single-actor) message)
  (ask (wrapped-actor self) message))

(defmacro %with-async-ask (actor message &rest body)
  "Macro that makes a `call', but asynchronous. Therefore it spawns a new single-actor which waits for the result.
The provided body is the response handler."
  (with-gensyms (self msg state waiter)
    `(let ((,waiter (make-single-actor 'actor
                                       :receive-fun (lambda (,self ,msg ,state)
                                                      (unwind-protect
                                                           (progn
                                                             (funcall ,@body ,msg)
                                                             (tell ,self :stop)
                                                             (cons ,msg ,state))
                                                        (tell ,self :stop)))
                                       :after-start-fun (lambda (,self ,state)
                                                          (declare (ignore ,state))
                                                          ;; this will call the `cast' function
                                                          (gs::submit-message ,actor ,message nil ,self))
                                       :name (gensym "Async-ask-waiter-"))))
       ,waiter)))

(defmethod async-ask ((self single-actor) message)
  "This returns a `future'.
An `async-ask' is similar to a `ask' in that the caller gets back a result 
but it doesn't have to actively wait for it. Instead a `future' wraps the result.
However, the internal message handling is based on `tell'.
How this works is that the message to the target `actor' is not 'sent' using the callers thread
but instead an anonymous `actor' is started behind the scenes and this in fact makes tells
the message to the target `actor'. It does sent itself along as 'sender'.
The target `actor' tells a response back to the initial `sender'. When that happens and the anonymous `actor'
received the response the `future' will be fulfilled with the `promise'."
  (make-future (lambda (promise-fun)
                 (log:debug "Executing fcomputation function...")
                 (%with-async-ask (wrapped-actor self) message
                                   (lambda (result)
                                     (log:debug "Result: ~a~%" result)
                                     (funcall promise-fun result))))))

(defun make-single-actor (clazz &key name state receive-fun after-start-fun queue-size)
  (assert (subtypep clazz 'actor))
  (let ((single-actor (make-instance 'single-actor)))
    (with-slots (wrapped-actor) single-actor
      (setf wrapped-actor (make-instance clazz :name name
                                               :state state
                                               :receive-fun receive-fun
                                               :after-start-fun after-start-fun))
      (with-slots (msgbox) wrapped-actor
        (setf msgbox (make-instance 'mb:message-box-bt
                                    :max-queue-size queue-size))))
    (after-init single-actor state)
    single-actor))

;; (defmacro with-actor (&rest body)
;;   (format t "body: ~a~%" body)
;;   (labels ((filter-fun (x) (equal (car x) 'receive)))
;;     (let ((recv-form (cdr (car (fset:filter #'filter-fun body))))
;;           (rest-body (remove-if #'filter-fun body))
;;           (actor-sym (gensym))
;;           (msg-sym (gensym))
;;           (state-sym (gensym)))
;;       `(make-actor "tmp-actor"
;;                    :state nil
;;                    :receive-fun (lambda (,actor-sym ,msg-sym ,state-sym)
;;                                   ,(let ((self actor-sym)
;;                                          (msg msg-sym)
;;                                          (state state-sym))
;;                                      (car recv-form)))
;;                    :after-init-fun (lambda (,actor-sym ,state-sym)
;;                                      ,(let ((self actor-sym)
;;                                             (state state-sym))
;;                                         (car rest-body)))))))
