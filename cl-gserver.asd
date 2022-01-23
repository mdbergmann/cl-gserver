(defsystem "cl-gserver"
  :version "1.11.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :description "Actor framework featuring actors and agents for easy access to state and asynchronous operations."
  :depends-on ("cl-gserver/logif"
               "cl-gserver/logif-simple"
               "cl-gserver/plain")
  :in-order-to ((test-op (test-op "cl-gserver/tests"))))

(defsystem "cl-gserver/plain"
  :author "Manfred Bergmann"
  :description "cl-gserver with no logging. combine with one of the logging interfaces."
  :depends-on ("alexandria"
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
                 (:file "actor-api")
                 (:file "dispatcher-api")
                 (:module "queue"
                  :components
                  ((:file "queue")))
                 (:module "mbox"
                  :components
                  ((:file "message-box")))
                 (:file "eventstream-api")
                 (:file "actor-system-api")
                 (:file "actor-context-api")
                 (:file "fcomputation")
                 (:file "actor-cell")
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
                   (:file "array-agent")))))))

(defsystem "cl-gserver/tests"
  :author "Manfred Bergmann"
  :depends-on ("cl-gserver"
               "fiveam"
               "cl-mock")
  :components ((:module "tests"
                :components
                ((:file "all-test")
                 (:file "logif-simple-test")
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
  :description "Test system for cl-gserver"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-gserver.tests))))


;; documentation

(defsystem "cl-gserver/docs"
  :author "Manfred Bergmann"
  :description "Documentation for cl-gserver"
  :depends-on ("cl-gserver"
               "mgl-pax")
  :components ((:file "documentation")))

;; logging interface

(defsystem "cl-gserver/logif"
  :author "Manfred Bergmann"
  :description "Logging interface api"
  :components ((:module "src"
                :components
                ((:module "logif"
                  :components
                  ((:file "logif")))))))

(defsystem "cl-gserver/logif-simple"
  :author "Manfred Bergmann"
  :description "Logging interface - simple implementation"
  :depends-on ("cl-gserver/logif")
  :components ((:module "src"
                :components
                ((:module "logif"
                  :components
                  ((:file "logif-simple")))))))

(defsystem "cl-gserver/logif-log4cl"
  :author "Manfred Bergmann"
  :description "Logging interface - log4cl implementation"
  :depends-on ("cl-gserver/logif" "log4cl")
  :components ((:module "src"
                :components
                ((:module "logif"
                  :components
                  ((:file "logif-log4cl")))))))

;; load system
;; (asdf:load-system "cl-gserver")
;;
;; test system
;; (asdf:test-system "cl-gserver/tests")
;;
;; (hlp:document (asdf:find-system :cl-gserver) :only-exported t)
;; (pax:update-asdf-system-html-docs cl-gserver.docs:@cl-gserver :cl-gserver :target-dir #P"~/docs/")

#|

TODOs:

|#

