
(in-package :sento.actor)

(shadowing-import '(act-cell:pre-start
                    act-cell:after-stop
                    act-cell:handle-call
                    act-cell:handle-cast
                    act-cell:stop
                    future:make-future
                    ev:subscribe
                    ev:unsubscribe
                    ev:publish
                    ac:find-actors
                    ac:all-actors
                    ac:actor-of))

;; we want to use symbols in this package rather than 'cell'.
(define-symbol-macro *self* act-cell:*self*)
(define-symbol-macro *state* act-cell:*state*)
(define-symbol-macro *sender* act-cell:*sender*)

(defmethod make-actor (receive &key name state (type 'actor) (init nil) (destroy nil))
  (make-instance type
                 :name name
                 :state state
                 :receive receive
                 :init init
                 :destroy destroy))

(defun finalize-initialization (actor message-box actor-context)
  "Private API: finalize initialization of the actor with a `mesgb:message-box` and an `ac:actor-context`."
  (setf (act-cell:msgbox actor) message-box)
  (setf (act:context actor) actor-context)
  (with-slots (init-fun) actor
    (when init-fun
      (funcall init-fun actor))))

(defmethod print-object ((obj actor) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (call-next-method obj string-stream)
      (format stream "path: ~a, cell: ~a"
              (path obj)
              (get-output-stream-string string-stream)))))

;; -------------------------------
;; actor-cell impls
;; -------------------------------

(defmethod handle-call ((self actor) message)
  (with-slots (receive behavior) self
    (let ((effective-behavior (if behavior behavior receive)))
      (funcall effective-behavior message))))

(defmethod handle-cast ((self actor) message)
  (with-slots (receive behavior) self
    (let ((effective-behavior (if behavior behavior receive)))
      (funcall effective-behavior message))))

(defmethod pre-start ((self actor))
  (call-next-method))

(defmethod after-stop ((self actor))
  (call-next-method)
  (with-slots (destroy-fun) self
    (when destroy-fun
      (funcall destroy-fun self))))

;; -------- children handling ----------

(defun stop-children (actor &optional (wait nil))
  (let ((context (context actor)))
    (when context
      (dolist (child (ac:all-actors context))
        (stop child wait)))))

(defun notify-watchers-about-stop (actor)
  (dolist (watcher (watchers actor))
    (typecase watcher
      (act:actor (tell watcher (cons :stopped actor)))
      (ac:actor-context (ac:notify watcher actor :stopped)))))

;; -------------------------------
;; actor protocol impl
;; -------------------------------

(defmethod tell ((self actor) message &optional sender)
  (act-cell:cast self message sender))

(defmethod ask-s ((self actor) message &key (time-out nil))
  (act-cell:call self message :time-out time-out))

(defmethod become (new-behavior)
  (with-slots (behavior) *self*
    (setf behavior new-behavior)))

(defmethod unbecome ()
  (with-slots (behavior) *self*
    (setf behavior nil)))

(defmethod path ((self actor))
  (when (context self)
    (ac:id (context self))))

(defmethod name ((self actor))
  (act-cell:name self))

(defmethod watch ((self actor) watcher)
  (with-slots (watchers) self
    (push watcher watchers)))

(defmethod unwatch ((self actor) watcher)
  (with-slots (watchers) self
    (setf watchers (utils:filter (lambda (w) (not (eq watcher w))) watchers))))

(defmethod stop ((self actor) &optional (wait nil))
  "If this actor has an `actor-context`, also stop all children.
In any case stop the actor-cell. See `actor-cell:stop` for more info on stopping."
  (stop-children self wait)
  (call-next-method self wait)
  (notify-watchers-about-stop self))

;; -------------------------------
;; Async handling
;; -------------------------------

(defclass async-waitor-actor (actor) ())

(defmacro with-waiting-actor (actor message system time-out &rest body)
  (alexandria:with-gensyms (msg msgbox waiting-actor)
    `(let ((,msgbox (if ,system
                        (make-instance 'mesgb:message-box/dp
                                       :name (string (gensym "waiter-mb/dp-"))
                                       :dispatcher
                                       (getf (asys:dispatchers ,system) :shared))
                        (make-instance 'mesgb:message-box/bt
                                       :name (string (gensym "waiter-mb/bt-")))))
           (,waiting-actor (make-instance
                            'async-waitor-actor
                            :receive (lambda (,msg)
                                        (unwind-protect
                                             (progn
                                               (funcall ,@body ,msg)
                                               (act-cell:stop *self*))
                                          (act-cell:stop *self*)))
                            :name (string (gensym "Ask-Waiter-")))))
       (setf (act-cell:msgbox ,waiting-actor) ,msgbox)
       (act-cell::submit-message ,actor ,message nil ,waiting-actor ,time-out)
       ,waiting-actor)))

(defmethod ask ((self actor) message &key (time-out nil))
  (future:make-future
   (lambda (promise-fun)
     (log:debug "Executing future function...")
     (let* ((context (context self))
            (system (if context (ac:system context) nil))
            (timed-out-p nil)
            (result-received-p nil)
            (waiting-actor nil))
       (flet ((handle-timeout (&optional cause)
                (log:info "Timeout condition: ~a" cause)
                (setf timed-out-p t)
                (funcall promise-fun
                         (cons :handler-error
                               (make-condition 'utils:ask-timeout
                                               :wait-time time-out
                                               :cause cause)))
                (tell waiting-actor :stop))
              (handle-error (&optional cause)
                (log:warn "~a" cause)
                (funcall promise-fun
                         (cons :handler-error cause))
                (tell waiting-actor :stop)))
         (setf waiting-actor
               (with-waiting-actor self message system time-out
                 (lambda (result)
                   (setf result-received-p t)
                   (log:info "Result: ~a, timed-out:~a" result timed-out-p)
                   (unless timed-out-p
                     (funcall promise-fun result)))))
         (when time-out
           (when system
             (handler-case
                 (wt:schedule (asys:timeout-timer system)
                              time-out
                              (lambda ()
                                (unless result-received-p
                                  (handle-timeout))))
               (error (c)
                 (handle-error c))))
           (unless system
             (handler-case
                 (utils:with-waitfor (time-out)
                   (utils:wait-cond (lambda () result-received-p) 0.1))
               (bt:timeout (c)
                 (handle-timeout c))))))))))

;; -------------------------------
;; eventstream protocol impl
;; -------------------------------

(defmethod subscribe ((actor actor) (subscriber actor) &optional pattern)
  "Convenience. Allows to subscribe to `ev:eventstream` by just providing the actor."
  (ev:subscribe (ac:system (context actor)) subscriber pattern))

(defmethod unsubscribe ((actor actor) (unsubscriber actor))
  "Convenience. Allows to unsubscribe to `ev:eventstream` by just providing the actor."
  (ev:unsubscribe (ac:system (context actor)) unsubscriber))

(defmethod publish ((actor actor) message)
  "Convenience. Allows to publish to `ev:eventstream` by just providing the actor."
  (ev:publish (ac:system (context actor)) message))


;; --------------------------------------
;; actor-context protocol impl (partial)
;; --------------------------------------

(defmethod find-actors ((actor actor) path &key (test #'string=) (key #'act-cell:name))
  "`ac:actor-context` protocol implementation."
  (ac:find-actors (context actor) path :test test :key key))

(defmethod all-actors ((actor actor))
  "`ac:actor-context` protocol implementation."
  (ac:all-actors (context actor)))

(defmethod actor-of ((actor actor)
                     &key receive
                       (init nil) (destroy nil)
                       (dispatcher :shared) (state nil)
                       (type 'act:actor) (name nil))
  "`ac:actor-context` protocol implementation"
  (ac:actor-of (context actor)
    :receive receive
    :init init
    :destroy destroy
    :dispatcher dispatcher
    :state state
    :type type
    :name name))
