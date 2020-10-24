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
                 (:module "core"
                          :components
                          ((:file "queue")
                           (:file "dispatcher-api")
                           (:file "message-box")
                           (:file "actor-cell")))
                 (:file "actor-system-api")
                 (:file "actor-context")
                 (:file "fcomputation")
                 (:file "actor")
                 (:file "agent")
                 (:file "dispatcher")
                 (:file "actor-system"))))
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
                 (:file "actor-cell-test")
                 (:file "actor-mp-test")
                 (:file "agent-test")
                 (:file "actor-test")
                 (:file "actor-context-test")
                 (:file "fcomputation-test")
                 (:file "dispatcher-test")
                 (:file "actor-system-test"))))
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
;; - add agent to system
;; - add 'after-stop' handler for cleaning up and remove from actor syste,
;; - add 'before-start' handler instead of 'before-start'
;; - see how to mingle in a 'pinned' dispatcher which uses the 'bt' based message box.
;; - cleanup all the doc strings

