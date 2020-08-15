(defsystem "cl-gserver"
  :version "0.6.0"
  :author "Manfred Bergmann"
  :license "MIT"
  :description "Erlang inspired GenServer library with Agent for easy access to state."
  :depends-on ("lparallel"
               "cl-speedy-queue"
               "log4cl"
               "iterate")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "queue")
                 (:file "message-box")
                 (:file "gserver")
                 (:file "agent")
                 (:file "actor"))))
  :in-order-to ((test-op (test-op "cl-gserver/tests"))))

(defsystem "cl-gserver/tests"
  :author "Manfred Bergmann"
  :license "MIT"
  :depends-on ("cl-gserver"
               "fiveam"
               "trivia")
  :components ((:module "tests"
                :components
                ((:file "all-test")
                 (:file "utils-test")
                 (:file "gserver-test")
                 (:file "gserver-mp-test")
                 (:file "agent-test")
                 (:file "actor-test"))))
  :description "Test system for cl-gserver"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-gserver.tests))))

;; add to asdf:*central-registry* is not done
;; (push #P"~/Development/MySources/cl-gserver/" asdf:*central-registry*)
;;
;; load system
;; (asdf:load-system "cl-gserver")
;;
;; test system
;; (asdf:test-system "cl-gserver/tests")

;; TODOs:
;; - test the actor macro
