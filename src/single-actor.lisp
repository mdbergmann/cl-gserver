(defpackage :cl-gserver.single-actor
  (:use :cl :cl-gserver.actor)
  (:nicknames :si-act)
  (:import-from #:cl-gserver.future
                #:make-future)
  (:import-from #:cl-gserver.actor-cell
                #:msgbox
                #:state)
  (:import-from #:mesgb
                #:message-box-bt)
  (:export #:make-single-actor
           #:single-actor
           #:single-actor-p))

(in-package :cl-gserver.single-actor)

;; -------------------------------------------------------
;; 'single' actor
;; -------------------------------------------------------

(defclass single-actor (wrapping-actor-base) ()
  (:documentation
   "A 'single' actor can be instantiated separate of a `system'.
It will run it's own threaded message-box."))

(defun single-actor-p (obj)
  (typep obj 'single-actor))

(defmethod async-ask ((self single-actor) message)
  (make-future (lambda (promise-fun)
                 (log:debug "Executing fcomputation function...")
                 (make-single-actor (%make-waitor-actor (the-wrapped self) message
                                   (lambda (result)
                                     (log:debug "Result: ~a~%" result)
                                     (funcall promise-fun result)))))))

(defun make-single-actor (creator-fun &key (queue-size 0))
  (let ((inner-actor (funcall creator-fun))
        (single-actor (make-instance 'single-actor)))

    (unless (typep inner-actor 'actor)
      (error "actor-class is no subclass of 'actor"))

    (setf (the-wrapped single-actor) inner-actor)

    (with-slots (msgbox state) (the-wrapped single-actor)
      (setf msgbox (make-instance 'message-box-bt
                                  :max-queue-size queue-size))
      (after-start single-actor state))
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
