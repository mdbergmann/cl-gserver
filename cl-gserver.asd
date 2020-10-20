(defsystem "cl-gserver"
  :version "0.7.0"
  :author "Manfred Bergmann"
  :license "MIT"
  :description "Erlang inspired GenServer library featuring actors and agents for easy access to state and asynchronous operations."
  :depends-on ("lparallel"
               "cl-speedy-queue"
               "log4cl"
               "iterate"
               "str"
               "blackbird")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "fcomputation")
                 (:file "queue")
                 (:file "dispatcher-api")
                 (:file "system-api")
                 (:file "message-box")
                 (:file "actor-context")
                 (:file "gserver")
                 (:file "agent")
                 (:file "actor")
                 (:file "single-actor")
                 (:file "dispatcher")
                 (:file "system"))))
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
                 (:file "actor-mp-test")
                 (:file "agent-test")
                 (:file "actor-test")
                 (:file "actor-context-test")
                 (:file "fcomputation-test")
                 (:file "dispatcher-test")
                 (:file "system-test"))))
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
