(defpackage :cl-gserver.gserver-mp-test
  (:use :cl :trivia :iterate :fiveam :cl-gserver)
  (:export #:run!
           #:all-tests
           #:nil))

(in-package :cl-gserver.gserver-mp-test)

(def-suite gserver-mp-tests
  :description "gserver mp tests"
  :in cl-gserver.tests:test-suite
  )

(in-suite gserver-mp-tests)

(log:config :warn)

;;(init-dispatcher-threadpool 1)


(test counter-mp
  "Counter server - multi processors."

  (defclass counter-server (gserver) ())
  (defmethod handle-cast ((server counter-server) message current-state)
    (cons current-state current-state))
  (defmethod handle-call ((server counter-server) message current-state)
    (match message
      (:add
       (let ((new-state (1+ current-state)))
         (cons new-state new-state)))
      (:sub
       (let ((new-state (1- current-state)))
         (cons new-state new-state)))
      (:get (cons current-state current-state))))

  (let ((cut (make-instance 'counter-server :state 0 :dispatch-workers 4)))
    (time
     (progn 
       (iter (repeat 1000)
         (call cut :add))
       (iter (repeat 500)
         (call cut :sub))))

    (is (= 500 (call cut :get))))
  )

  (run! 'counter-mp)
