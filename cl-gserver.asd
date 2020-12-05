(defsystem "cl-gserver"
  :version "1.1.2"
  :author "Manfred Bergmann"
  :license "MIT"
  :description "Actor framework featuring actors and agents for easy access to state and asynchronous operations."
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
                 (:file "router")
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
                 (:file "router-test")
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
=> Allow no cons result from behavior function / add sender to receive context
- do error on async-wait if there are no dispatcher workers
- check what to do with the 'waiter-actor'. should it also use 'actor-of'?
OK - implement router
OK - write new readme
OK - make performance tests
OK - fix the package problem on SBCL
OK - make sure that actor names are unique within their context.
OK - add 'behavior' instead of 'behavior' to actor and 'become' to switch the behavior.
OK - stopping an actor removes it from the parent actor context
OK - find a way to cancel the execution for async-ask on shared dispatcher
OK - use original bt, but make wrapper for bt:with-timeout and catch different timeout conditions.
OK - think about cancellation of dispatched jobs
OK - async-ask timeout should be measured from within the waiting actor.
OK - add timeouts for ask, async-ask and all cases that use those.
OK - add watching (parent to child) which will notify of termination
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

