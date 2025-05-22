(defsystem "sento"
  :version "3.4.2"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :description "Actor framework featuring actors and agents for easy access to state and asynchronous operations."
  :depends-on ("alexandria"
               "log4cl"
               "bordeaux-threads"
               "cl-speedy-queue"
               "str"
               "blackbird"
               "binding-arrows"
               "timer-wheel"
               "local-time-duration"
               #-abcl "atomics"
               )
  :components ((:module "src"
                :serial t
                :components
                ((:module "atomic"
                  :components
                  ((:file "atomic-api")
                   #-abcl (:file "atomic")
                   #+abcl (:file "atomic-abcl")))
                 (:file "config")
                 (:file "wheel-timer")
                 (:file "timeutils")
                 (:file "miscutils")
                 (:file "fcomputation")
                 (:file "dispatcher-api")
                 (:module "queue"
                  :components
                  ((:file "queue")
                   (:file "queue-locked")
                   ;;#+sbcl (:file "queue-sbcl")
                   ))
                 (:module "mbox"
                  :components
                  ((:file "message-box")))
                 (:file "actor-cell")
                 (:file "actor-api")
                 (:file "eventstream-api")
                 (:file "actor-system-api")
                 (:file "actor-context-api")
                 (:file "fasync-completed")
                 (:file "actor")
                 (:file "agent")
                 (:file "eventstream")
                 (:file "fsm")
                 (:file "tasks")
                 (:file "router")
                 (:file "stash")
                 (:file "dispatcher")
                 (:file "actor-context")
                 (:file "actor-system")
                 (:module "agent-usecase"
                  :components
                  ((:file "agent-usecase-commons")
                   (:file "hash-agent")
                   (:file "array-agent")))
                 (:file "package"))))
  :in-order-to ((test-op (test-op "sento/tests"))))

(defsystem "sento/tests"
  :author "Manfred Bergmann"
  :depends-on ("sento"
               "fiveam"
               "serapeum"
               "lparallel"
               "cl-mock")
  :components ((:module "tests"
                :serial t
                :components
                ((:file "all-test")
                 (:file "miscutils-test")
                 (:file "timeutils-test")
                 (:file "atomic-test")
                 (:file "config-test")
                 (:file "wheel-timer-test")
                 (:file "bounded-queue-test")
                 (:file "unbounded-queue-test")
                 (:file "actor-cell-test")
                 (:file "actor-mp-test")
                 (:file "agent-test")
                 (:file "hash-agent-test")
                 (:file "array-agent-test")
                 (:file "actor-test")
                 (:file "fsm-test")
                 (:file "router-test")
                 (:file "stash-test")
                 (:file "tasks-test")
                 (:file "eventstream-test")
                 (:file "actor-context-test")
                 (:file "fcomputation-test")
                 (:file "fasync-completed-test")
                 (:file "dispatcher-test")
                 (:file "actor-system-test")
                 (:file "actor-tree-test")
                 (:file "spawn-in-receive-test")
                 (:file "test-utils")
                 (:file "message-box-test"))))
  :description "Test system for sento"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:sento.tests))))

;; --------------------------------
;; documentation
;; --------------------------------

(defsystem "sento/docs"
  :author "Manfred Bergmann"
  :description "Documentation for sento"
  :depends-on ("sento"
               "mgl-pax/full")
  :components ((:file "documentation")))


;; --------------------------------
;; benchmark
;; --------------------------------

(defsystem "sento/bench"
  :author "Manfred Bergmann"
  :description "Benchmark for Sento"
  :depends-on ("sento"
               "serapeum"
               "trivial-benchmark"
               "trivial-garbage")
  :components ((:file "bench")))

;; load system
;; (asdf:load-system "sento")
;;
;; test system
;; (asdf:test-system "sento/tests")
;;
;; (hlp:document (asdf:find-system :sento) :only-exported t)
;; (pax:update-asdf-system-html-docs sento.docs::@sento :sento :target-dir #P"~/docs/")

#|


|#

