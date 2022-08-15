(defsystem "sento"
  :version "1.13.0"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :description "Actor framework featuring actors and agents for easy access to state and asynchronous operations."
  :depends-on ("alexandria"
               "log4cl"
               "bordeaux-threads"
               "lparallel"
               "cl-speedy-queue"
               "str"
               "blackbird"
               "binding-arrows"
               "timer-wheel"
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
                 (:file "utils")
                 (:file "dispatcher-api")
                 (:module "queue"
                  :components
                  ((:file "queue")))
                 (:module "mbox"
                  :components
                  ((:file "message-box")))
                 (:file "actor-cell")
                 (:file "actor-api")
                 (:file "eventstream-api")
                 (:file "actor-system-api")
                 (:file "actor-context-api")
                 (:file "fcomputation")
                 (:file "actor")
                 (:file "agent")
                 (:file "eventstream")
                 (:file "tasks")
                 (:file "router")
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
               "cl-mock")
  :components ((:module "tests"
                :components
                ((:file "all-test")
                 (:file "atomic-test")
                 (:file "config-test")
                 (:file "wheel-timer-test")
                 (:file "utils-test")
                 (:file "actor-cell-test")
                 (:file "actor-mp-test")
                 (:file "agent-test")
                 (:file "hash-agent-test")
                 (:file "array-agent-test")
                 (:file "actor-test")
                 (:file "router-test")
                 (:file "tasks-test")
                 (:file "eventstream-test")
                 (:file "actor-context-test")
                 (:file "fcomputation-test")
                 (:file "dispatcher-test")
                 (:file "actor-system-test")
                 (:file "actor-tree-test")
                 (:file "spawn-in-receive-test")
                 )))
  :description "Test system for sento"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:sento.tests))))


;; documentation

(defsystem "sento/docs"
  :author "Manfred Bergmann"
  :description "Documentation for sento"
  :depends-on ("sento"
               "mgl-pax")
  :components ((:file "documentation")))


;; load system
;; (asdf:load-system "sento")
;;
;; test system
;; (asdf:test-system "sento/tests")
;;
;; (hlp:document (asdf:find-system :sento) :only-exported t)
;; (pax:update-asdf-system-html-docs sento.docs:@sento :sento :target-dir #P"~/docs/")

#|

TODOs:

|#

