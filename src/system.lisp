(defpackage :cl-gserver.system
  (:use :cl :cl-gserver.actor :cl-gserver.future
        :cl-gserver.system-api :cl-gserver.actor-context)
  (:nicknames :system)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:dispatcher-api
                #:make-dispatcher)
  (:import-from #:dispatcher
                #:dispatcher-bt)
  (:import-from #:gs
                #:msgbox)
  (:import-from #:mesgb
                #:message-box-dp)
  (:export #:make-system
           #:system
           #:get-system
           #:system-actor-p))

(in-package :cl-gserver.system)

(defclass system (actor-context)
  ((dispatcher :initarg :dispatcher
               :initform nil
               :reader get-dispatcher
               :documentation "The message dispatcher."))
  (:documentation
   "A system is a container for `actors' or subclasses."))

(defmethod print-object ((obj system) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (dispatcher) obj
      (format stream "dispatcher: ~a"
              dispatcher))))

(defun make-system (&key (num-workers 4))
  "Creates a system with 4 workers by default."
  (make-instance 'system
                 :dispatcher (make-dispatcher 'dispatcher-bt :num-workers num-workers)))

(defmethod shutdown ((self system))
  (dispatcher-api:shutdown (get-dispatcher self)))

;; -------------------------------------
;; actor-context impl
;; -------------------------------------

(defmethod actor-of ((self system) create-fun)
  (let ((system-actor (make-system-actor self create-fun)))
    (add-actor self system-actor)
    system-actor))

;; --------------------------------------
;; system actor
;; --------------------------------------

(defclass system-actor ()
  ((wrapped-actor :initform nil
                  :accessor the-wrapped
                  :documentation "The wrapped actor. `actor' acts as a facade.")
   (system :initarg :system
           :initform nil
           :type 'system
           :reader get-system
           :documentation "The system of this actor."))
  (:documentation
   "A 'system' actor is instanciated as part of a `system'.
The message dispatch in this case works using a shared thread pool."))

(defun system-actor-p (obj)
  (typep obj 'system-actor))

(defmethod print-object ((obj system-actor) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wrapped-actor system) obj
      (format stream "system: ~a, wrapped: ~a" system wrapped-actor))))

(defmethod after-start ((self system-actor) state)
  (after-start (the-wrapped self) state))

(defmethod tell ((self system-actor) message)
  (tell (the-wrapped self) message))

(defmethod ask ((self system-actor) message)
  (ask (the-wrapped self) message))

(defmacro %with-async-ask (actor message system &rest body)
  "Macro that makes a `call', but asynchronous. Therefore it spawns a new system-actor which waits for the result.
The provided body is the response handler."
  (with-gensyms (self msg state waiter)
    `(let ((,waiter (make-system-actor
                     ,system
                     (lambda ()
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
                                                         ;; this will call the `cast' function
                                                         (gs::submit-message ,actor ,message nil ,self))
                                      :name (string (gensym "Async-ask-waiter-")))))))
       ,waiter)))

(defmethod async-ask ((self system-actor) message)
  (make-future (lambda (promise-fun)
                 (log:debug "Executing fcomputation function...")
                 (%with-async-ask (the-wrapped self) message (get-system self)
                                   (lambda (result)
                                     (log:debug "Result: ~a~%" result)
                                     (funcall promise-fun result))))))

(defun make-system-actor (system creator-fun)
  (let ((inner-actor (funcall creator-fun))
        (system-actor (make-instance 'system-actor :system system)))

    (unless (typep inner-actor 'actor)
      (error "actor-class is no subclass of 'actor"))

    (with-slots (wrapped-actor) system-actor
      (setf wrapped-actor inner-actor)
      
      (with-slots (msgbox) wrapped-actor
        (setf msgbox (make-instance 'message-box-dp
                                    :dispatcher (get-dispatcher system)))))
    system-actor))
