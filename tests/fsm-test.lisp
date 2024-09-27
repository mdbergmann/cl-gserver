(defpackage :sento.fsm-test
  (:use :cl :fiveam :sento.fsm)
  (:import-from #:act
                #:tell)
  (:import-from #:miscutils
                #:await-cond)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.fsm-test)

(def-suite fsm-tests
           :description "Tests for finite-state-machine"
           :in sento.tests:test-suite)

(in-suite fsm-tests)

(log:config :warn)

(def-fixture test-fsm (start-with
                          event-handling-fun)
  (let ((asys (asys:make-actor-system '(:dispatchers (:shared (:workers 1))))))
    (unwind-protect
         (let ((cut (make-fsm asys
                              :name "test"
                              :start-with start-with
                              :event-handling event-handling-fun)))
           (log:config :debug)
           (&body))
      (progn
        (log:config :warn)
        (ac:shutdown asys)))))

(test create-fsm--with-state
  "Creates a finite-state-machine actor, with state only, no data."
  (let ((asys (asys:make-actor-system '(:dispatchers (:shared (:workers 1))))))
    (unwind-protect
         (let ((cut (make-fsm asys
                              :name "foo"
                              :start-with '(start . nil))))
           (is (not (null cut)))
           (is (equal "foo" (act-cell:name cut)))
           (is (equalp (fsm::make-fsm-state :state 'start :data nil)
                       (act-cell:state cut))))
      (ac:shutdown asys))))

(test fsm-event-handling--goto
  "Tests transition to another state."
  (let ((event-handling-fun
          (lambda ()
            (when-state ('start)
              (on-event ('next)
                (goto-state 'initialized)))
            (when-state ('initialized)
              (on-event ('deinit)
                (goto-state 'uninitialized)))
            )))
    (with-fixture test-fsm
        ('(start . nil)
         event-handling-fun)
      (tell cut 'next)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'initialized :data nil)
                         (act-cell:state cut))))
      (tell cut 'deinit)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'uninitialized :data nil)
                         (act-cell:state cut)))))))

(test fms-event-handling--with-state-timeout--triggered-by-goto
  (let ((event-handling-fun
          (lambda ()
            (when-state ('start)
              (on-event ('next)
                (goto-state 'initialized)))
            (when-state ('initialized :timeout-s 1)
              (on-event ('deinit) :state-timeout
                (goto-state 'uninitialized)))
            )))
    (with-fixture test-fsm
        ('(start . nil)
         event-handling-fun)
      (tell cut 'next)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'initialized :data nil)
                         (act-cell:state cut))))
      ;; event 'de-init is not sent, but automatically triggered by timeout
      (is-true (await-cond 1.5
                 (equalp (fsm::make-fsm-state :state 'uninitialized :data nil)
                         (act-cell:state cut)))))))

(test fsm-event-handling--stay-with-adding-data
  "Tests an event handling that doesn't change state but updates state data"
  (let ((event-handling-fun
          (lambda ()
            (when-state ('start)
              (on-event ('init-data)
                (stay-on-state *event-data*)))
            (when-state ('start)
              (on-event ('next) (goto-state 'active)))
            (when-state ('active)
              (on-event ('push-data)
                (stay-on-state (concatenate 'vector *state-data* *event-data*))))
            )))
    (with-fixture test-fsm
        ('(start . nil)
         event-handling-fun)
      (tell cut '(init-data . #(1 2 3)))
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'start :data #(1 2 3))
                         (act-cell:state cut))))
      (tell cut 'next)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'active :data #(1 2 3))
                         (act-cell:state cut))))
      (tell cut '(push-data . #(4)))
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'active :data #(1 2 3 4))
                         (act-cell:state cut)))))))

(test fms-event-handling--with-state-timeout--stay-should-not-trigger-timeout
  (let ((event-handling-fun
          (lambda ()
            (when-state ('initialized :timeout-s 1)
              (on-event ('deinit) :state-timeout
                (goto-state 'uninitialized)))
            (when-unhandled ('do-stay)
              (stay-on-state))
            )))
    (with-fixture test-fsm
        ('(initialized . nil)
         event-handling-fun)
      (tell cut 'do-stay)
      (sleep 1.5)
      (is (equalp (fsm::make-fsm-state :state 'initialized :data nil)
                  (act-cell:state cut))))))

(test fsm-unhandled-events
  (let ((event-handling-fun
          (lambda ()
            (on-event ('end)
              ;; do nothing, counts as unhandled
              )
            (on-event ('handle)
              ;; handle it here, so should not call `when-unhandled' below.
              (goto-state 'handled))
            
            (when-unhandled ('end)
              (goto-state 'unhandled))
            (when-unhandled ('handle)
              (goto-state 'no-should-handle))
            )))
    (with-fixture test-fsm
        ('(start . nil)
         event-handling-fun)
      (tell cut 'end)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'unhandled :data nil)
                         (act-cell:state cut))))
      (tell cut 'handle)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'handled :data nil)
                         (act-cell:state cut)))))))

(test fsm-transitions
  "Tests on transition operations"
  (let* ((transitioned-to-active nil)
         (transitioned-to-same nil)
         (invalid-transition-called nil)
         (event-handling-fun
           (lambda ()
             (when-state ('start)
               (on-event ('next)
                 (goto-state 'active))
               (on-event ('same)
                 (goto-state 'start)))
             (on-transition ('(start . active))
               (setf transitioned-to-active t))
             (on-transition ('(start . start))
               (setf transitioned-to-same t))
             (on-transition ('(start . foo))
               (setf invalid-transition-called t))
             )))
    (with-fixture test-fsm
        ('(start . nil)
         event-handling-fun)
      (tell cut 'same)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'start :data nil)
                         (act-cell:state cut))))
      (is-true (await-cond 0.5
                 transitioned-to-same))
      (tell cut 'next)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'active :data nil)
                         (act-cell:state cut))))
      (is-true (await-cond 0.5
                 transitioned-to-active))
      (is-false invalid-transition-called))))

(test fsm-transitions--stay-on-same--does-not-exec-transition
  (let* ((transitioned-to-same nil)
         (event-handling-fun
           (lambda ()
             (when-state ('start)
               (on-event ('same)
                 (stay-on-state)))
             (on-transition ('(start . start))
               (setf transitioned-to-same t))
             )))
    (with-fixture test-fsm
        ('(start . nil)
         event-handling-fun)
      (tell cut 'same)
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'start :data nil)
                         (act-cell:state cut))))
      (sleep 0.2)
      (is-false transitioned-to-same))))

(test fsm-transitions--check--state-data--and--next-state-data
  (let* ((state-data nil)
         (next-state-data nil)
         (event-handling-fun
           (lambda ()
             (when-state ('start)
               (on-event ('next)
                 (goto-state 'active *event-data*)))
             (on-transition ('(start . active))
               (setf state-data *state-data*
                     next-state-data *next-state-data*))
             )))
    (with-fixture test-fsm
        ('(start . (1 2 3))
         event-handling-fun)
      (tell cut '(next . (4 5 6)))
      (is-true (await-cond 0.5
                 (equalp (fsm::make-fsm-state :state 'active :data '(4 5 6))
                         (act-cell:state cut))))
      (is-true (await-cond 0.5
                 (and (equalp state-data '(1 2 3))
                      (equalp next-state-data '(4 5 6))))))))

;; ------------------------------------
;; full example with test
;; ------------------------------------

(defclass buncher (fsm) ())

(defun make-buncher (ac handler-fun)
  (make-fsm ac :name "buncher"
               :start-with '(idle . uninitialized)
               :event-handling handler-fun
               :type 'buncher))

;; events/messages
(defstruct set-target ref)
(defstruct todo ref vec)
(defstruct queue num)
(defstruct batch vec)

(def-fixture buncher-common (event-handler-fun)
  (let ((asys (asys:make-actor-system)))
    (unwind-protect
         (let* ((buncher (make-buncher asys event-handler-fun))
                (received-msg nil)
                (sender-ref (ac:actor-of asys
                                         :receive
                                         (lambda (msg)
                                           (setf received-msg msg)))))
           (log:config :debug)
           (&body))
      (progn
        (log:config :warn)
        (ac:shutdown asys)))))

(defun buncher-event-handling--complex-types ()
  (when-state ('idle)
    (on-event ('set-target :test #'typep)
      (stay-on-state (make-todo :ref (set-target-ref *received-event*)
                                :vec #()))))

  (when-state ('active :timeout-s 1)
    (on-event ('flush) :state-timeout
      (let ((new-data (copy-todo *state-data*)))
        (setf (todo-vec new-data) #())
        (goto-state 'idle new-data))))
    
  (when-unhandled ('queue :test #'typep)
    (goto-state 'active
        (let ((new-data (copy-todo *state-data*))
              (vec (todo-vec *state-data*)))
          (setf (todo-vec new-data)
                (concatenate 'vector
                             vec
                             `#(,(queue-num *received-event*))))
          new-data)))

  (on-transition ('(active . idle))
    (when (typep *state-data* 'todo)
      (let ((ref (todo-ref *state-data*))
            (vec (todo-vec *state-data*)))
        (tell ref (make-batch :vec vec))))))

(test buncher--with-complex-types
  (with-fixture buncher-common (#'buncher-event-handling--complex-types)
    (tell buncher (make-set-target :ref sender-ref))
    (tell buncher (make-queue :num 42))
    (tell buncher (make-queue :num 43))
    (is-true (await-cond 1.2
               (and (batch-p received-msg)
                    (equalp #(42 43) (batch-vec received-msg)))))
    (tell buncher (make-queue :num 44))
    (tell buncher 'flush)
    (is-true (await-cond 0.5
               (and (batch-p received-msg)
                    (equalp #(44) (batch-vec received-msg)))))
    (tell buncher (make-queue :num 45))
    (is-true (await-cond 1.2
               (and (batch-p received-msg)
                    (equalp #(45) (batch-vec received-msg)))))
    ))

;; -----------------------------------------------------------------

;; Same example as above but by utilizing the `cons' event type with `cdr' as data.
;; Can lead to less complicated event handling
(defun buncher-event-handling--explicit-data ()
  (when-state ('idle)
    (on-event ('set-target)
      (stay-on-state `(,*event-data* . #()))))

  (when-state ('active)
    (on-event ('flush)
      (goto-state 'idle `(,(car *state-data*) . #()))))
    
  (when-unhandled ('queue)
    (goto-state 'active
        `(,(car *state-data*) .
          ,(concatenate 'vector
                        (cdr *state-data*)
                        `#(,*event-data*)))))

  (on-transition ('(active . idle))
    (when (consp *state-data*)
      (tell (car *state-data*) (cdr *state-data*)))))

(test buncher--with-simple-types--but-explicit-data  
  (with-fixture buncher-common (#'buncher-event-handling--explicit-data)
    (tell buncher `(set-target . ,sender-ref))
    (tell buncher '(queue . 42))
    (tell buncher '(queue . 43))
    (tell buncher 'flush)
    (is-true (await-cond 0.5
               (equalp #(42 43) received-msg)))
    (tell buncher '(queue . 44))
    (tell buncher 'flush)
    (is-true (await-cond 0.5
               (equalp #(44) received-msg)))))

;;(run! 'fsm-tests)

;; TODOs:
;; - documentation
