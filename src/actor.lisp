(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver.actor-cell)
  (:nicknames :act)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:future
                #:make-future)
  (:export #:actor
           #:tell
           #:ask
           #:async-ask
           #:before-start
           #:system)
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
(defgeneric before-start (actor state)
  (:documentation
   "Generic function definition that you may call from `initialize-instance'."))
(defgeneric system (actor)
  (:documentation "Access to the `actor-system'."))

(defclass actor (actor-api actor-cell)
  ((receive-fun :initarg :receive-fun
                :initform (error "'receive-fun' must be specified!")
                :reader receive-fun)
   (before-start-fun :initarg :before-start-fun
                    :initform nil
                    :reader before-start-fun
                    :documentation
                    "Code to be called after actor start.
The `before-start-fun' lambda takes two arguments. 
1: the actor instance, 
2: the state"))
  (:documentation
   "This is the `actor' class.
The `actor' does it's message handling in the `receive' function.
There is asynchronous `tell' and synchronous `ask'.
To stop an actors message processing in order to cleanup resouces you should tell (either `tell' or `ask')
the `:stop' message. It will respond with `:stopped'."))

(defmethod initialize-instance :after ((self actor) &key)
  (log:debug "After initialize: ~a" self)
  (with-slots (before-start-fun act-cell:state) self
    (when before-start-fun
      (funcall before-start-fun self act-cell:state))))

(defmethod handle-call ((self actor) message state)
  (funcall (receive-fun self) self message state))
(defmethod handle-cast ((self actor) message state)
  (funcall (receive-fun self) self message state))

(defmethod system ((self actor))
  (when (next-method-p)
    (call-next-method)))

(defmethod tell ((self actor) message)
  (cast self message))
(defmethod ask ((self actor) message)
  (call self message))

(defmacro with-waitor-actor (actor message system &rest body)
  (with-gensyms (self msg state msgbox)
    `(let ((,msgbox (if ,system
                        (make-instance 'mesgb:message-box-dp
                                       :dispatcher (system-api:message-dispatcher ,system))
                        (make-instance 'mesgb:message-box-bt))))
       (make-instance 'actor 
                      :receive-fun (lambda (,self ,msg ,state)
                                     (unwind-protect
                                          (progn
                                            (funcall ,@body ,msg)
                                            (tell ,self :stop)
                                            (cons ,msg ,state))
                                       (tell ,self :stop)))
                      :before-start-fun (lambda (,self ,state)
                                         (declare (ignore ,state))
                                         ;; this will call the `tell' function
                                         (act-cell::submit-message ,actor ,message nil ,self))
                      :name (string (gensym "Async-ask-waiter-"))
                      :msgbox ,msgbox))))

(defmethod async-ask ((self actor) message)
  (make-future (lambda (promise-fun)
                 (log:debug "Executing future function...")
                 (with-waitor-actor self message (act:system self)
                   (lambda (result)
                     (log:debug "Result: ~a~%" result)
                     (funcall promise-fun result))))))

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
;;                    :before-start-fun (lambda (,actor-sym ,state-sym)
;;                                      ,(let ((self actor-sym)
;;                                             (state state-sym))
;;                                         (car rest-body)))))))
