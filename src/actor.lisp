(defpackage :cl-gserver.actor
  (:use :cl :cl-gserver.gserver)
  (:nicknames :act)
  (:export #:actor
           #:receive
           #:tell
           #:ask
           #:async-ask
           #:make-actor)
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


(defclass actor-base (gserver)
  ((receive-fun :initarg :receive-fun
                :initform (error "Must be specified!")
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

(defmethod initialize-instance :after ((self actor-base) &key)
  (log:debug "Initialize instance: ~a~%" self))

(defmethod handle-cast ((self actor-base) message current-state)
  (funcall (receive-fun self) self message current-state))
(defmethod handle-call ((self actor-base) message current-state)
  (funcall (receive-fun self) self message current-state))
(defmethod after-init ((self actor-base) state)
  (with-slots (after-start-fun) self
    (when after-start-fun
      (funcall after-start-fun self state))))

(defmethod tell ((self actor-base) message)
  (cast self message))

(defmethod ask ((self actor-base) message)
  (call self message))

(defmethod async-ask ((self actor-base) message)
  ;; use the underlying `async-call' from gserver.
  (async-call self message))


;; -------------------------------------------------------
;; 'standard' actor
;; -------------------------------------------------------

(defclass actor ()
  ((wrapped-actor :initform nil
                  :reader wrapped-actor
                  :documentation "The wrapped actor. `actor' acts as a facade.")
   (name :initarg :name
         :initform (string (gensym "act-"))
         :type string)
   (state :initarg :state
          :initform nil)
   (max-queue-size :initarg :max-queue-size
                   :initform 0
                   :type integer
                   :documentation "0 means unbounded queue. > 0 means bounded queue. For bounded choose >= 100")
   (receive-fun :initarg :receive-fun
                :initform (error "Must be specified!")
                :documentation
                "The `receive' method handles all messages to an `actor' being it `tell' or `ask'.
But the convention persists that the result of `receive' must be a `cons' where
`car' is to be returned to the caller (for `ask') and `cdr' will update the state.")
   (after-start-fun :initarg :after-start-fun
                    :initform nil
                    :documentation "Code to be called after actor start.")))

(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wrapped-actor) obj
      (format stream "wrapped: ~a" wrapped-actor))))

(defmethod initialize-instance :after ((self actor) &key)
  (with-slots (wrapped-actor name state max-queue-size receive-fun after-start-fun) self
    (setf wrapped-actor (make-instance 'actor-base
                                       :name name
                                       :state state
                                       :max-queue-size max-queue-size
                                       :receive-fun receive-fun
                                       :after-start-fun after-start-fun))))

(defmethod tell ((self actor) message)
  (tell (wrapped-actor self) message))

(defmethod ask ((self actor) message)
  (ask (wrapped-actor self) message))

(defmethod async-ask ((self actor) message)
  (async-ask (wrapped-actor self) message))

(defun make-actor (&key
                     (name (gensym "actor-"))
                     state
                     receive-fun
                     after-start-fun)
  "Makes a new 'standard' `actor' which allows you to specify 
a name with `:state', `:receive-fun' and `:after-start-fun'."
  (make-instance 'actor :name name
                        :state state
                        :receive-fun receive-fun
                        :after-start-fun after-start-fun))

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
