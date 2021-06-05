(defpackage :cl-gserver.actor-mp-test
  (:use :cl :fiveam :act)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.actor-mp-test)

(def-suite actor-mp-tests
  :description "actor mp tests"
  :in cl-gserver.tests:test-suite)

(in-suite actor-mp-tests)

(log:config :warn)

(defparameter *receive-fun* (lambda (self message current-state)
                              (declare (ignore self))
                              (cond
                                ((eq :add message)
                                 (let ((new-state (1+ current-state)))
                                   (cons new-state new-state)))
                                ((eq :sub message)
                                 (let ((new-state (1- current-state)))
                                   (cons new-state new-state)))
                                ((eq :get message)
                                 (cons current-state current-state)))))

(def-fixture mp-setup (queue-size pinned shared)
  (setf lparallel:*kernel* (lparallel:make-kernel 8))

  (defclass counter-actor (actor) ())

  (when pinned
    (run-pinned-test queue-size
      (&body)))
  
  (when shared
    (run-system-test :random
      (&body))
    (run-system-test :round-robin
      (&body)))
  
  (lparallel:end-kernel))

(defmacro run-pinned-test (queue-size &body body)
  `(progn
     (format t "~%Running non-system tests (~a)...~%" ,queue-size)
     (let* ((cut (make-instance 'counter-actor
                                :name "counter-actor"
                                :state 0
                                :receive *receive-fun*))
            (max-loop 10000)
            (per-thread (/ max-loop 8)))
       (setf (act-cell:msgbox cut) (make-instance 'mesgb:message-box/bt :max-queue-size ,queue-size))
       ,@body
       (act-cell:stop cut))
     (format t "Running non-system tests (~a)...done~%" ,queue-size)))

(defmacro run-system-test (dispatcher-strategy &body body)
  `(progn
    (format t "Running system tests (~a)...~%" ,dispatcher-strategy)
    (let* ((system (asys:make-actor-system '(:dispatchers (:shared (:workers 4
                                                                    :strategy ,dispatcher-strategy)))))
           (cut (ac:actor-of system (lambda ()
                                      (make-instance 'counter-actor :state 0
                                                                    :receive *receive-fun*))))
           (max-loop 10000)
           (per-thread (/ max-loop 8)))
      (unwind-protect
           ,@body
        (act-cell:stop cut)
        (ac:shutdown system)))
    (format t "Running system tests (~a)...done~%" ,dispatcher-strategy)))

(test counter-mp-unbounded
  "Counter server - multi processors - unbounded queue"

  (with-fixture mp-setup (nil t t)
    (map nil #'lparallel:force
         (mapcar (lambda (x)
                   (declare (ignore x))
                   (lparallel:future
                     (dotimes (n (1+ per-thread))
                       (ask-s cut :add))
                     (dotimes (n per-thread)
                       (ask-s cut :sub))))
                 (loop repeat 8 collect "n")))
    (is (= 8 (ask-s cut :get)))))

(test counter-mp-unbounded--mixed
  "Counter server - multi processors - unbounded queue - mixed ask-s and ask"

  (with-fixture mp-setup (nil nil t)
    (mapcar (lambda (x)
              (declare (ignore x))
              (bt:make-thread
               (lambda ()
                 (loop :repeat (1+ per-thread)
                       :for async = (random 2)
                       :if (= async 1)
                         :do (ask cut :add)
                       :else
                         :do (ask-s cut :add))
                 (loop :repeat per-thread
                       :for async = (random 2)
                       :if (= async 1)
                         :do (ask cut :sub)
                       :else
                         :do (ask-s cut :sub)))))
            (loop repeat 8 collect "n"))
    (is (utils:assert-cond (lambda () (= 8 (ask-s cut :get))) 3 0.2))))

(test counter-mp-bounded
  "Counter server - multi processors - bounded queue"

  (with-fixture mp-setup (100 t t)
    (map nil #'lparallel:force
         (mapcar (lambda (x)
                   (declare (ignore x))
                   (lparallel:future
                     (dotimes (n (1+ per-thread))
                       (ask-s cut :add))
                     (dotimes (n per-thread)
                       (ask-s cut :sub))))
                 (loop repeat 8 collect "n")))
    (is (= 8 (ask-s cut :get)))))

(defun run-tests ()
  (time (run! 'counter-mp-unbounded))
  (time (run! 'counter-mp-unbounded--mixed))
  (time (run! 'counter-mp-bounded)))
