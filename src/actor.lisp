(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver.actor-cell)
  (:nicknames :act)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:future
                #:make-future)
  (:export #:make-actor
           #:actor
           #:tell
           #:ask
           #:async-ask
           #:context)
  ;;#:with-actor)
  )

(in-package :cl-gserver.actor)


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


(defclass actor (actor-cell actor-api)
  ((receive-fun :initarg :receive-fun
                :initform (error "'receive-fun' must be specified!")
                :reader receive-fun)
   (context :initform nil
            :accessor context
            :documentation
            "This is the `actor-context' every actor carries.
When the actor is created from scratch it has no `actor-context'.
When created through the contexts, or systems `actor-of' function an `actor-context' will be set."))
  (:documentation
   "This is the `actor' class.
The `actor' does it's message handling in the `receive' function.
There is asynchronous `tell' and synchronous `ask'.
To stop an actors message processing in order to cleanup resouces you should tell (either `tell' or `ask')
the `:stop' message. It will respond with `:stopped'."))

(defun make-actor (receive-fun &key name state)
  (make-instance 'actor
                 :name name
                 :state state
                 :receive-fun receive-fun))

(defmethod handle-call ((self actor) message state)
  (funcall (receive-fun self) self message state))
(defmethod handle-cast ((self actor) message state)
  (funcall (receive-fun self) self message state))

(defmethod tell ((self actor) message)
  (cast self message))
(defmethod ask ((self actor) message)
  (call self message))

;; -------------------------------
;; Async handling
;; -------------------------------

(defclass async-waitor-actor (actor)
  ((after-start-fun :initarg :after-start-fun)))

(defmethod before-start ((self async-waitor-actor) state)
  (when (next-method-p)
    (call-next-method))
  (with-slots (after-start-fun) self
    (funcall after-start-fun self state)))

(defmacro with-waitor-actor (actor message system &rest body)
  (with-gensyms (self msg state msgbox waitor-actor)
    `(let ((,msgbox (if ,system
                        (make-instance 'mesgb:message-box-dp
                                       :dispatcher
                                       (getf (asys:dispatchers ,system) :shared))
                        (make-instance 'mesgb:message-box-bt)))
           (,waitor-actor (make-instance 'async-waitor-actor
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
                                                           (act-cell::submit-message ,actor ,message nil ,self))
                                        :name (string (gensym "Async-ask-waiter-")))))
       (setf (act-cell:msgbox ,waitor-actor) ,msgbox))))

(defmethod async-ask ((self actor) message)
  (make-future (lambda (promise-fun)
                 (log:debug "Executing future function...")
                 (let ((context (context self)))
                   (with-waitor-actor self message (if context (ac:system context) nil)
                                      (lambda (result)
                                        (log:debug "Result: ~a~%" result)
                                        (funcall promise-fun result)))))))

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
