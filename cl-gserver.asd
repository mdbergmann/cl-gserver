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
                ((:file "dispatcher-api")
                 (:file "actor-system-api")
                 (:file "actor-context-api")
                 (:file "actor-api")
                 (:file "utils")
                 (:module "core"
                  :components
                  ((:file "queue")
                   (:file "message-box")
                   (:file "actor-cell")))
                 (:file "fcomputation")
                 (:file "actor")
                 (:file "agent")
                 (:file "dispatcher")
                 (:file "actor-context")
                 (:file "actor-system"))))
  :in-order-to ((test-op (test-op "cl-gserver/tests"))))

(defsystem "cl-gserver/tests"
  :author "Manfred Bergmann"
  :license "MIT"
  :depends-on ("cl-gserver"
               "fiveam"
               "cl-mock"
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

;; load system
;; (asdf:load-system "cl-gserver")
;;
;; test system
;; (asdf:test-system "cl-gserver/tests")

#|
TODOs:
=> - stopping an actor removes it from the actors in the context
- check what to do with the 'waiter-actor'. should it also use 'actor-of'?
- add agent to system
- add 'after-stop' handler for cleaning up and remove from actor syste,
- make sure that actor names are unique within their context.
- add timeouts for ask, async-ask and all cases that use those.
OK - shutdown of actor-context should stop actors and recursively also shutdown child acs
OK - add 'stop' actor to the actor-context protocol.
OK - add actor-context as composition to system (twice for 'user' and 'system') and actor
OK - extract actor api to separate api
OK - cleanup all the doc strings
OK - stop 'user' actors first on shutdown
OK - verify system shutdown
OK - add 'pre-start' handler instead of 'pre-start'
OK - see how to mingle in a 'pinned' dispatcher which uses the 'bt' based message box.
OK - put all '-api' packages first in asd
|#

