(defpackage :sento.actor-cell
  (:use :cl :sento.utils)
  (:nicknames :act-cell)
  (:import-from #:mesgb
                #:inner-msg
                #:cancelled-p
                #:with-submit-handler)
  (:export #:actor-cell
           #:cell
           #:name
           #:msgbox
           #:state
           #:*state*
           #:*self*
           #:*sender*
           ;; API
           #:handle-call
           #:handle-cast
           #:pre-start
           #:after-stop
           #:stop
           #:call
           #:cast
           #:running-p))

(in-package :sento.actor-cell)

(defvar *self* nil
  "The 'own' actor instance. Dynamically bound and available upon calling `receive` function.")
(defvar *state* nil
  "The 'state' of the actor. Dynamically bound and available in `receive` function.")
(defvar *sender* nil
  "The `*sender*` is dynamically bound and available in `receive` function, when it is known.")

(defstruct actor-cell-state (running t :type boolean))

(defclass actor-cell ()
   ((name :initarg :name
          :initform (string (gensym "actor-"))
          :reader name
          :documentation
          "The name of the actor/actor-cell. If no name is specified a default one is applied.")
    (state :initarg :state
           :initform nil
           :reader state
           :documentation
           "The encapsulated state.")
    (internal-state :initform (make-actor-cell-state)
                    :documentation
                    "The internal state of the server.")
    (msgbox :initform nil
            :accessor msgbox
            :documentation
            "The `message-box`. By default the `actor`/`actor-cell` has no message-box.
When the actor is created through the `actor-context` of an actor, or the `actor-system`
then it will be populated with a message-box."))
  (:documentation
   "`actor-cell` is the base of the `actor`.
It encapsulates state and can executes async operations.
State can be changed by `setf`ing `*state*` special variable from inside `receive` function, via calling `call` or `cast`.
Where `call` is waiting for a result and `cast` does not.
For each `call` and `cast` handlers must be implemented by subclasses.

It uses a `message-box` to processes the received messages.
When the `actor`/`actor-cell` was created ad-hoc (out of the `actor-system`/`actor-context`),
it will not have a message-box and can't process messages.
When the `actor` is created through the `actor-system` or `actor-context`,
one can decide what kind of message-box/dispatcher should be used for the new `actor`.

See `actor-context` `actor-of` method for more information on this.

To stop an `actor` message handling and you can send the `:stop` message 
either via `call` (which will respond with `:stopped`) or `cast`.
This is to cleanup thread resources when the actor is not needed anymore.

Note: the `actor-cell` uses `call` and `cast` functions which translate to `ask-s` and `tell` on the `actor`."))

(defmethod print-object ((obj actor-cell) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name state internal-state msgbox) obj
      (format stream "~a, running: ~a, state: ~a, message-box: ~a"
              name
              (slot-value internal-state 'running)
              state
              msgbox))))

(defmethod initialize-instance :after ((obj actor-cell) &key)
  (with-slots (name state) obj
    (unless name
      (setf name (string (gensym "actor-"))))
    (log:debug "~a initialized: ~a" name obj)
    (let ((*state* state))
      (pre-start obj)
      (setf state *state*))))

;; -----------------------------------------------
;; public functions / API
;; -----------------------------------------------

(defgeneric pre-start (actor-cell)
  (:documentation
   "Generic function definition that called from `initialize-instance`."))

(defgeneric after-stop (actor-cell)
  (:documentation
   "Generic function definition that is called after the actor cell has stopped."))

(defgeneric handle-call (actor-cell message)
  (:documentation
   "Handles calls to the server. Must be implemented by subclasses.
The result of the last expression of this function is returned back to the 'caller'.
State of the cell can be changed via `setf`ing `*state*` variable."))

(defgeneric handle-cast (actor-cell message)
  (:documentation
   "Handles casts to the server. Must be implemented by subclasses.
State of the cell can be changed via `setf`ing `*state*` variable."))

(defgeneric stop (actor-cell &optional wait)
  (:documentation "Stops the actor-cells message processing gracefully.
This is not an immediate stop.  
There are two ways to stop an actor (cell).

1. by calling this function.
It is not an immediate stop. The actor will finish the current message processing.  
`wait`: waits until the cell is stopped.  

2. by sending `:stop` to the actor (cell).
This won't allow to wait when the actor is stopped, even not with `ask-s`.
The `:stop` message (symbol) is normally processed by the actors message processing."))

;; ---------------------------------
;; Impl
;; ---------------------------------

(defmethod pre-start ((self actor-cell))
  "Empty implementation so that we can call it anyway even if there are no other implementations."
  (declare (ignore state))
  nil)

(defmethod after-stop ((self actor-cell))
  "Empty implementation so that we can call it anyway even if there are no other implementations."
  nil)

(defun call (actor-cell message &key (time-out nil))
  "Send a message to a actor-cell instance and wait for a result.
Specify a timeout in seconds if you require a result within a certain period of time.
Be aware though that this is a resource intensive wait based on a waiting thread.
The result can be of different types.
Normal result: the last expression of `handle-call` (or `receive` in `act:actor`) implementation. 
Error result: `(cons :handler-error <condition>)'
In case of time-out the error condition is a bt:timeout."
  (when message
    (let ((result (submit-message actor-cell message t nil time-out)))
      (log:debug "~a: message process result: ~a" (name actor-cell) result)
      result)))

(defun cast (actor-cell message &optional sender)
  "Sends a message to a actor-cell asynchronously. There is no result.
If a `sender' is specified the result will be sent to the sender."
  (when message
    (let ((result (submit-message actor-cell message nil sender nil)))
      (log:debug "~a: message process result: ~a" (name actor-cell) result)
      result)))  

(defun running-p (actor-cell)
  "Returns true if this server is running. `nil` otherwise."
  (with-slots (internal-state) actor-cell
    (slot-value internal-state 'running)))

(defmethod stop ((self actor-cell) &optional (wait nil))
  (log:debug "~a: stopping on actor-cell: ~a" (name self) self)
  (with-slots (msgbox internal-state) self
    (when (slot-value internal-state 'running)
      (setf (slot-value internal-state 'running) nil)
      (when msgbox
        (mesgb:stop msgbox wait))
      (after-stop self))))

;; -----------------------------------------------    
;; internal functions
;; -----------------------------------------------

(defmacro with-sender (sender &rest body)
  `(let ((*sender* ,sender))
     ,@body))

(defun submit-message (actor-cell message withreply-p sender time-out)
  "Submitting a message.
In case of `withreply-p`, the `response` is filled because submitting to the message-box is synchronous.
Otherwise submitting is asynchronous and `response` is just `t`.
In case the actor-cell was stopped it will respond with just `:stopped`.
In case no messge-box is configured this function respnds with `:no-message-handling`."
  (log:debug "~a: submitting message: ~a, withreply-p: ~a, sender: ~a, timeout: ~a"
             (name actor-cell) message withreply-p sender time-out)

  (with-slots (internal-state msgbox) actor-cell
    (unless (actor-cell-state-running internal-state)
      (return-from submit-message :stopped))
    (unless msgbox
      (return-from submit-message :no-message-handling)))

  (handler-case
      (with-submit-handler
          ((slot-value actor-cell 'msgbox)
           message
           withreply-p
           time-out)
          (with-sender sender
            (handle-message actor-cell message withreply-p)))
    (utils:ask-timeout (c)
      (log:warn "~a: ask-s timeout: ~a" (name actor-cell) c)
      (cons :handler-error c))))

;; ------------------------------------------------
;; message handling ---------------------
;; ------------------------------------------------

(defun handle-message (actor-cell message withreply-p)
  "This function is submitted as `handler-fun` to message-box."
  (log:debug "~a: handling message: ~a" (name actor-cell) message)
  (handler-case
      (let ((internal-handle-result (handle-message-internal actor-cell message)))
        (case internal-handle-result
          (:resume
           (with-slots (state) actor-cell
             (let* ((*self* actor-cell)
                    (*state* state)
                    (handle-result
                      (handle-message-user actor-cell message withreply-p)))
               (setf state *state*)
               handle-result)))
          (t internal-handle-result)))
    (t (c)
      (log:error "~a: error condition was raised: ~%~a~%"
                 (name actor-cell)
                 c)
      (cons :handler-error c))))

(defun handle-message-internal (actor-cell message)
  "A `:stop` message will response with `:stopping` and the user handlers are not called.
Otherwise the result is `:resume` to resume user message handling."
  (log:debug "~a: internal handle-message: ~a" (name actor-cell) message)
  (case message
    (:stop (progn
             (stop actor-cell)
             :stopped))
    (t :resume)))

(defun handle-message-user (actor-cell message withreply-p)
  "The user defined message handler.
Effectively this calls the `handle-call` or `handle-cast` functions."
  (log:debug "~a: user handle message: ~a" (name actor-cell) message)
  (if withreply-p
      (handle-call actor-cell message)
      (handle-cast actor-cell message)))
