(defpackage :cl-gserver.single-actor
  (:use :cl :cl-gserver.actor :cl-gserver.future)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:cl-gserver.gserver
                #:msgbox)
  (:nicknames :si-act)
  (:export #:make-single-actor
           #:single-actor
           #:async-ask
           #:the-wrapped))

(in-package :cl-gserver.single-actor)

;; -------------------------------------------------------
;; 'single' actor
;; -------------------------------------------------------

(defclass single-actor ()
  ((wrapped-actor :initform nil
                  :accessor the-wrapped
                  :documentation "The wrapped actor. `actor' acts as a facade."))
  (:documentation
   "A 'single' actor can be instantiated separate of a `system'.
It will run it's own threaded message-box."))

(defmethod print-object ((obj single-actor) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wrapped-actor) obj
      (format stream "wrapped: ~a" wrapped-actor))))

(defmethod after-start ((self single-actor) state)
  (after-start (the-wrapped self) state))

(defmethod tell ((self single-actor) message)
  (tell (the-wrapped self) message))

(defmethod ask ((self single-actor) message)
  (ask (the-wrapped self) message))

(defmacro %with-async-ask (actor message &rest body)
  "Macro that makes an `ask', but asynchronous. Therefore it spawns a new single-actor which waits for the result.
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
                                       :name (string (gensym "Async-ask-waiter-")))))
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
                 (%with-async-ask (the-wrapped self) message
                                   (lambda (result)
                                     (log:debug "Result: ~a~%" result)
                                     (funcall promise-fun result))))))

(defun make-single-actor (actor-class &key name state receive-fun after-start-fun queue-size)
  (unless (subtypep actor-class 'actor)
    (error "actor-class is no subclass of 'actor"))
  (let ((single-actor (make-instance 'single-actor)))
    (with-slots (wrapped-actor) single-actor
      (setf wrapped-actor (make-instance actor-class :name name
                                                     :state state
                                                     :receive-fun receive-fun
                                                     :after-start-fun after-start-fun))
      (with-slots (msgbox) wrapped-actor
        (setf msgbox (make-instance 'mesgb:message-box-bt
                                    :max-queue-size queue-size))))
    (after-start single-actor state)
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
;;                    :after-start-fun (lambda (,actor-sym ,state-sym)
;;                                      ,(let ((self actor-sym)
;;                                             (state state-sym))
;;                                         (car rest-body)))))))
