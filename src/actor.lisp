(defpackage :cl-gserver.actor
  (:use :cl)
  (:nicknames :act)
  (:import-from #:act-cell
                #:actor-cell
                #:before-start
                #:after-stop
                #:handle-call
                #:handle-cast
                #:stop)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:future
                #:make-future))

(in-package :cl-gserver.actor)

(defclass actor (actor-cell)
  ((receive-fun :initarg :receive-fun
                :initform (error "'receive-fun' must be specified!")
                :reader receive-fun)
   (context :initform nil
            :accessor context))
  (:documentation
   "This is the `actor' class.
The `actor' does it's message handling in the `receive' function.
There is asynchronous `tell' and synchronous `ask'.
To stop an actors message processing in order to cleanup resouces you should tell (either `tell' or `ask')
the `:stop' message. It will respond with `:stopped'."))

(defmethod make-actor (receive-fun &key name state)
  (make-instance 'actor
                 :name name
                 :state state
                 :receive-fun receive-fun))

;; -------------------------------
;; actor-cell impls
;; -------------------------------

(defmethod handle-call ((self actor) message state)
  (funcall (receive-fun self) self message state))
(defmethod handle-cast ((self actor) message state)
  (funcall (receive-fun self) self message state))

(defmethod stop ((self actor))
  "If this actor has an `actor-context', also stop all children.
In any case stop the actor-cell."
  (let ((context (context self)))
    (when context
        (dolist (child (ac:all-actors context))
          (stop child)))
    (call-next-method)))

;; -------------------------------
;; actor protocol impl
;; -------------------------------

(defmethod tell ((self actor) message)
  (act-cell:cast self message))
(defmethod ask ((self actor) message)
  (act-cell:call self message))

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
                        (make-instance 'mesgb:message-box/dp
                                       :dispatcher
                                       (getf (asys:dispatchers ,system) :shared))
                        (make-instance 'mesgb:message-box/bt)))
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
