(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver)
  (:nicknames :act)
  (:export #:actor
           #:receive
           #:send
           #:ask
           #:async-ask
           #:make-actor)
           ;;#:with-actor)
  )

(in-package :cl-gserver.actor)

(defclass actor (gserver) ()
  (:documentation
   "Specialized `gserver' class called `actor'.
There is a different terminology behind `actor'.
I.e. There is only one `receive' function.
And there is asynchronous `send' and synchronous `ask'.
So there is not much difference to a `gserver'.
It only uses one method `receive'. However both `handle-call' and `handle-cast' of `gserver'
end up in `receive'.
To stop an actors message processing in order to cleanup resouces you should send (either `send' or `ask')
the `:stop' message. It will respond with `:stopped'."))

(defmethod initialize-instance :after ((self actor) &key)
  (log:debug "Initialize instance: ~a~%" self))

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
  "Sends a message to the `actor'. `send' is asynchronous. There is no result."
  (cast actor message))

(defun ask (actor message)
  "Sends a message to the `actor'. `ask' is synchronous and waits for a result."
  (call actor message))

(defun async-ask (actor message)
  "Sends a message to `actor' and waits for a result but asynchronously.
The result is an `fcomputation' which accepts `on-complete' handlers, etc."
  ;; use the underlying `async-call' from gserver.
  (async-call actor message))

;; --------------------
;; Simple actor
;; --------------------
(defclass simple-actor (actor)
  ((receive-fun :initarg :receive-fun
                :initform nil
                :documentation "The receive function specified as slot.")
   (after-init-fun :initarg :after-init-fun
                   :initform nil
                   :documentation "Code to be called after actor start."))
  (:documentation
   "A simplified actor that can be created with just `make-actor'."))

(defmethod initialize-instance :after ((self simple-actor) &key)
  (log:debug "Initialize instance: ~a~%" self)
  (after-init self (slot-value self 'cl-gserver::state)))

(defmethod after-init ((self simple-actor) state)
  (with-slots (after-init-fun) self
    (when after-init-fun
      (funcall after-init-fun self state))))

(defmethod receive ((self simple-actor) message current-state)
  (with-slots (receive-fun) self
    (when receive-fun
      (funcall receive-fun self message current-state))))


(defun make-actor (&key
                     (name (utils:mkstr "actor-" (gensym)))
                     state
                     system
                     receive-fun
                     after-init-fun)
  "Makes a new `simple-actor' which allows you to specify 
a name with `:state', `:receive-fun' and `:after-init-fun'."
  (make-instance 'simple-actor :name name
                               :state state
                               :system system
                               :receive-fun receive-fun
                               :after-init-fun after-init-fun))

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
