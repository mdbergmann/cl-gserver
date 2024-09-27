(defpackage :sento.fsm
  (:use :cl :act)
  (:nicknames :fsm)
  (:export #:make-fsm
           #:fsm
           #:when-state
           #:on-event
           #:goto-state
           #:stay-on-state
           #:when-unhandled
           #:on-transition
           #:*state-data*
           #:*next-state-data*
           #:*received-event*
           #:*event-data*)
  )

(in-package :sento.fsm)

(defclass fsm (actor)
  ((event-handling-fun :initarg :event-handling
                       :initform nil
                       :reader event-handling-fun)
   (timeouts :initform (make-hash-table :test #'eq)
             :reader timeouts))
  (:documentation "Foo"))

(defstruct fsm-state
  (state)
  (data))

(defvar *current-state* nil "dynamically binds the current state")
(defvar *received-event* nil "dynamically binds the received event (msg)")
(defvar *event-was-handled-by-goto* nil)
(defvar *event-was-handled-by-stay* nil)

(defvar *event-data* nil "dynamically bound event data when msg was sent with data (`cons')")
(defvar *state-data* nil "dynamically bound data to the state data.")
(defvar *next-state-data* nil
  "dynamically bound data to the next state data (`on-transition').
Effectively same as `*event-data*' but should be used in different context.")

(defmacro when-state ((state &key (test '#'eq) timeout-s) &body body)
  `(progn
     (when ,timeout-s
       (setf (gethash ,state (timeouts *self*)) ,timeout-s))
     (when (funcall ,test *current-state* ,state)
       (when ,timeout-s
         (log:debug "Registered timeout for state: ~a" ,state))
       ,@body)))

(defmacro on-event ((event &key (test '#'eq)) &body body)
  (let ((declares-timeout (find :state-timeout body)))
    `(when (or (funcall ,test *received-event* ,event)
               (and ,declares-timeout
                    (eq *received-event* :state-timeout)))
       ,@body)))

(defmacro goto-state (next-state &optional (data nil data-p))
  `(progn
     (setf (fsm-state-state *state*) ,next-state)
     (when ,data-p
       (setf (fsm-state-data *state*) ,data))
     (setf *event-was-handled-by-goto* t)))

(defmacro stay-on-state (&optional (data nil data-p))
  `(progn
     (when ,data-p
       (setf (fsm-state-data *state*) ,data))
     (setf *event-was-handled-by-stay* t)))

(defmacro when-unhandled ((event &key (test '#'eq)) &body body)
  `(unless (or *event-was-handled-by-stay* *event-was-handled-by-goto*)
     (when (funcall ,test *received-event* ,event)
       ,@body)))

(defmacro on-transition ((transition &key (test '#'eq)) &body body)
  `(when (and *event-was-handled-by-goto*
              (funcall ,test
                       *current-state*
                       (car ,transition))
              (funcall ,test
                       (fsm-state-state *state*)
                       (cdr ,transition)))
     ,@body))

(defun %receive (msg)
  (with-slots (event-handling-fun timeouts) *self*
    (when event-handling-fun
      (let* ((*current-state* (fsm-state-state *state*))
             (*state-data* (fsm-state-data *state*))
             (*received-event* (if (consp msg)
                                   (car msg)
                                   msg))
             (*event-data* (if (consp msg)
                               (cdr msg)
                               nil))
             (*next-state-data* *event-data*)
             (*event-was-handled-by-stay* nil)
             (*event-was-handled-by-goto* nil))
        (log:debug "Current state: ~a, event: ~a" *current-state* msg)
        (log:debug "Current data: ~a, event-data: ~a, next-data: ~a" *state-data* *event-data* *next-state-data*)
        (handler-case
            (funcall event-handling-fun)
          (error (c)
            (log:warn "Error in event handler: ~a" c)))
        (log:debug "Last state: ~a, new state: ~a" *current-state* (fsm-state-state *state*))
        (log:debug "New data: ~a" *state-data*)

        (%setup-timeouts timeouts)))))

(defun %setup-timeouts (timeouts)
  (let* ((new-state (fsm-state-state *state*))
         (timeout (gethash new-state timeouts)))
    (when (and timeout
               *event-was-handled-by-goto*)
      (log:debug "State change with timeout. Scheduling timeout for state: ~a" new-state)
      (let ((scheduler (asys:scheduler (ac:system *self*)))
            (self *self*))
        (wt:schedule-once scheduler
                          timeout
                          (lambda ()
                            (log:debug "Timeout elapsed, sending :state-timeout.")
                            (! self :state-timeout)))))))

(defun make-fsm (actor-context &key name start-with event-handling
                                 (type 'fsm)
                                 (dispatcher-id :shared))
  (check-type name string)
  (check-type start-with list)
  (check-type event-handling (or null function))
  (ac:actor-of actor-context
               :type type
               :name name
               :receive (lambda (msg)
                          (%receive msg))
               :state (make-fsm-state :state (car start-with)
                                      :data (cdr start-with))
               :event-handling event-handling
               :dispatcher dispatcher-id))
