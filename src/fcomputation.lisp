(defpackage :sento.future
  (:use :cl)
  (:nicknames :future)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:future
           #:with-fut
           #:with-fut-resolve
           #:make-future
           #:futurep
           #:complete-p
           #:error-p
           #:fcompleted
           #:fawait
           #:fresult
           #:frecover
           #:fmap
           #:fresolve))

(in-package :sento.future)

(defclass future ()
  ((lock :initform (bt2:make-lock :name "future-lock")
         :documentation "Guards state transitions and callback registration.")
   (state :initform :pending
          :documentation "One of `:pending`, `:finished` or `:errored`.
Transitions exactly once, from `:pending` to either `:finished` or `:errored`.")
   (result :initform nil
           :documentation "The list of result values when `:finished`,
or the condition when `:errored`.")
   (callbacks :initform nil
              :documentation "Completion callbacks registered while `:pending`,
in reverse registration order. Each is called with the list of result values.")
   (errbacks :initform nil
             :documentation "Error callbacks registered while `:pending`,
in reverse registration order. Each is called with the condition."))
  (:documentation
   "A `future` is a delayed computation that eventually completes with a result or an error.
Completion handlers can be attached via `fcompleted`, chained computations via `fmap`,
errors recovered via `frecover`.
The implementation is thread-safe: a future may be resolved from any thread while
handlers are attached from other threads. Handlers are never invoked while the
future's internal lock is held, so handler code may freely operate on futures itself."))

(defmethod print-object ((obj future) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (state result) obj
      (format stream "state: ~a, result: ~a" state result))))

;; ---------------------------------
;; internal completion mechanics
;; ---------------------------------

(defun %run-guarded (fun arg)
  "Calls FUN with ARG, logging and absorbing any `serious-condition`.
An error signaled from a handler must not unwind into the thread that
resolves the future."
  (handler-case
      (funcall fun arg)
    (serious-condition (c)
      (log:warn "Error in future handler: ~a" c))))

(defun %complete (future values)
  "Transitions FUTURE to `:finished` with VALUES (a list) and runs the registered
completion callbacks. Only the first completion or error wins, later attempts are
ignored. Callbacks run on the calling thread, outside the future's lock."
  (with-slots (lock state result callbacks errbacks) future
    (let ((to-run nil))
      (bt2:with-lock-held (lock)
        (unless (eq state :pending)
          (log:debug "Ignoring completion of already resolved future, state: ~a" state)
          (return-from %complete future))
        (setf state :finished
              result values
              to-run (reverse callbacks)
              callbacks nil
              errbacks nil))
      (dolist (cb to-run)
        (%run-guarded cb values))))
  future)

(defun %fail (future condition)
  "Transitions FUTURE to `:errored` with CONDITION and runs the registered
error callbacks. Only the first completion or error wins, later attempts are
ignored. Callbacks run on the calling thread, outside the future's lock."
  (with-slots (lock state result callbacks errbacks) future
    (let ((to-run nil))
      (bt2:with-lock-held (lock)
        (unless (eq state :pending)
          (log:debug "Ignoring error on already resolved future, state: ~a" state)
          (return-from %fail future))
        (setf state :errored
              result condition
              to-run (reverse errbacks)
              callbacks nil
              errbacks nil))
      (dolist (eb to-run)
        (%run-guarded eb condition))))
  future)

(defun %on-completed (future callback)
  "Registers CALLBACK on FUTURE, called with the list of result values.
If FUTURE is already `:finished` the callback runs immediately on the calling
thread. On an `:errored` future the callback is dropped."
  (with-slots (lock state result callbacks) future
    (let ((run-now nil)
          (run-values nil))
      (bt2:with-lock-held (lock)
        (case state
          (:pending (push callback callbacks))
          (:finished (setf run-now t
                           run-values result))))
      (when run-now
        (%run-guarded callback run-values))))
  future)

(defun %on-error (future errback)
  "Registers ERRBACK on FUTURE, called with the condition.
If FUTURE is already `:errored` the errback runs immediately on the calling
thread. On a `:finished` future the errback is dropped."
  (with-slots (lock state result errbacks) future
    (let ((run-now nil)
          (run-condition nil))
      (bt2:with-lock-held (lock)
        (case state
          (:pending (push errback errbacks))
          (:errored (setf run-now t
                          run-condition result))))
      (when run-now
        (%run-guarded errback run-condition))))
  future)

(defun %chain (source target)
  "Completes the TARGET future with the SOURCE future's outcome once SOURCE resolves."
  (%on-completed source (lambda (values) (%complete target values)))
  (%on-error source (lambda (condition) (%fail target condition))))

;; ---------------------------------
;; public functions / API
;; ---------------------------------

(defmacro with-fut (&body body)
  "Convenience macro for creating a `future`.

The `future` will be resolved with the result of the body form."
  `(make-future (lambda (resolve-fun)
                  (let ((result (progn ,@body)))
                    (funcall resolve-fun result)))))

(defmacro with-fut-resolve (&body body)
  "Convenience macro for creating a `future` that must be resolved manually via `fresolve`.

This allows to spawn threads or other asynchronous operations as part of `body`.
However, you have to `resolve` the future eventually by applying a result on `resolve`.

Example:

```
(with-fut-resolve
  (bt2:make-thread
   (lambda ()
     (let ((result (do-some-lengthy-calculation)))
       (fresolve result)))))
```
"
  `(make-future (lambda (resolve-fun)
                  (macrolet ((fresolve (resolve-value)
                               `(funcall resolve-fun ,resolve-value)))
                     ,@body))))

(defun make-future (execute-fun)
  "Creates a future. `execute-fun` is the function that is executed immediately when the future is created.
`execute-fun` takes a parameter which is the `resolve-fun` function. Calling `resolve-fun` with a value
fulfills the future. Completing the future can happen right within `execute-fun`, or at any later time,
from any thread. In a multi-threaded environment `execute-fun` could spawn a thread, in which case
`execute-fun` returns immediately and the future is resolved later from the spawned thread.
If `execute-fun` signals an error the future completes as errored with that condition.

Create a future with:

```elisp
(make-future (lambda (resolve-fun)
               (let ((result (delayed-computation)))
                 (bt2:make-thread (lambda ()
                   (sleep 0.5)
                   (funcall resolve-fun result))))))
```
"
  (let* ((future (make-instance 'future))
         (resolve-fun (lambda (&rest values)
                        (%complete future values))))
    (handler-case
        (funcall execute-fun resolve-fun)
      (error (c)
        (log:debug "Error executing future function: ~a" c)
        (%fail future c)))
    future))

(defun futurep (object)
  "Checks if type of `object` is `future`."
  (typep object 'future))

(defun complete-p (future)
  "Is `future` completed? Returns either `t` or `nil`.
A future that errored also counts as completed, nothing will happen
with such a future anymore."
  (with-slots (lock state) future
    (bt2:with-lock-held (lock)
      (not (eq state :pending)))))

(defun error-p (future)
  "Is `future` errored? Returns either `t` or `nil`."
  (with-slots (lock state) future
    (bt2:with-lock-held (lock)
      (eq state :errored))))

(defun %fcompleted (future completed-fun)
  (%on-completed future
                 (lambda (values)
                   (funcall completed-fun (car values))))
  future)

(defmacro fcompleted (future (result) &body body)
  "Completion handler on the given `future`.

If the `future` is already complete then the `body` executes immediately.
`result` represents the future result.
`body` is executed when future completed.
Returns the future.

Notes on execution context:
By calling `fcompleted` a completion function is installed on the `future`.
If the `future` is already complete at that time, then `body` is called by the thread calling `fcompleted`.
Otherwise `body` is executed by the thread that resolves the `future`.
On an errored future the completion function is not called.

Example:

```
(fcompleted (with-fut
              (sleep .5)
              1)
            (result)
  (format t \"Future result ~a~%\" result))
```
"
  `(%fcompleted ,future (lambda (,result) ,@body)))

(defun fawait (fut &key timeout (sleep-time 0.1))
  "Wait for the future `FUT` to be completed. Returns `VALUES` with `result` of the future and `FUT`.
If the future is not completed after `TIMEOUT` seconds the `result` is `NIL`.
Note that a future completed with a `NIL` result also returns `NIL` (immediately) -
use `complete-p` to distinguish the two cases.
The `SLEEP-TIME` parameter specifies the time to sleep between checks of the future completion.
The wait is based on attempts. To be accurate in terms of `TIMEOUT` the `SLEEP-TIME` should be a divisor of `TIMEOUT`.
Disclaimer: naive implementation. There may be better solutions."
  (assert (and timeout (>= timeout 0)) (timeout) "Timeout must be greater or equal to 0")
  (let ((attempts (truncate timeout sleep-time)))
    (loop :repeat attempts
          :until (complete-p fut)
          :do (sleep sleep-time)))
  (values (when (complete-p fut)
            (fresult fut))
          fut))

(defun fresult (future)
  "Get the computation result. If not yet available `:not-ready` is returned.
A future completed with `NIL` returns `NIL` (check for completion with `complete-p`).
If the future errored, the condition is returned."
  (with-slots (lock state result) future
    (bt2:with-lock-held (lock)
      (case state
        (:pending :not-ready)
        (:errored result)
        (:finished (car result))))))

(defun %frecover (future handler-fun)
  "Returns a new future that completes like FUTURE. If FUTURE errors,
HANDLER-FUN is called with the condition and its return value completes the
new future. If HANDLER-FUN returns `%not-handled` the error is propagated.
If HANDLER-FUN itself signals a condition the new future errors with it."
  (let ((next-future (make-instance 'future)))
    (%on-completed future
                   (lambda (values)
                     (%complete next-future values)))
    (%on-error future
               (lambda (condition)
                 (handler-case
                     (let ((handler-result
                             (multiple-value-list (funcall handler-fun condition))))
                       (cond
                         ((eq (car handler-result) '%not-handled)
                          (%fail next-future condition))
                         ((futurep (car handler-result))
                          (%chain (car handler-result) next-future))
                         (t
                          (%complete next-future handler-result))))
                   (serious-condition (c)
                     (%fail next-future c)))))
    next-future))

(defmacro frecover (future &rest handler-forms)
  "Catch errors in futures using `frecover`
It works similar to `handler-case`.
Returns a new future. If the given `future` (or a chain of mapped futures) errors,
the condition is matched against `handler-forms` (as by `typecase`) and the result
of the matching handler completes the returned future.
A condition not matching any handler propagates as error to the returned future.

Example:

```
(fresult
 (frecover
  (-> (with-fut 0)
    (fmap (value)
      (declare (ignore value))
      (error \"foo\")))
    (fmap (value)
      (+ value 1))))
  (error (c) (format nil \"~a\" c))))
```
"
  (with-gensyms (condition)
    `(%frecover ,future
                (lambda (,condition)
                  (typecase ,condition
                    ,@(loop :for (typ binding . handler-body) :in handler-forms
                            :collect `(,typ ,(if binding
                                                 `(let ((,(car binding) ,condition))
                                                    ,@handler-body)
                                                 `(progn ,@handler-body))))
                    (t '%not-handled))))))

(defun %fmap (future map-fun)
  (let ((next-future (make-instance 'future)))
    (%on-error future
               (lambda (condition)
                 (%fail next-future condition)))
    (%on-completed future
                   (lambda (values)
                     (handler-case
                         (let ((map-result
                                 (multiple-value-list (funcall map-fun (car values)))))
                           (if (futurep (car map-result))
                               (%chain (car map-result) next-future)
                               (%complete next-future map-result)))
                       (serious-condition (c)
                         (%fail next-future c)))))
    next-future))

(defmacro fmap (future (result) &body body)
  "`fmap` maps a future.

`future` is the future that is mapped.
`result` is the result of the future when it completed.
`body` is the form that executes when the future is completed. The result of `body` generates a new future.
If `body` results in a `future` it is 'flattened', i.e. the mapped future completes with the
inner future's result. Errors, signaled by `body` or propagated from `future`, transition the
mapped future to errored state and can be handled with `frecover`.

Notes on execution context:
By calling `fmap` a mapping function is installed on the `future`.
If the `future` is already complete at that time, then `body` is called by the thread calling `fmap`.
Otherwise `body` is executed by the thread that resolves the `future`.


Example:

```
(fmap (with-fut 0) (result)
  (1+ result))
```
"
  `(%fmap ,future (lambda (,result) ,@body)))
