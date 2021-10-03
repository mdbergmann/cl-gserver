(defsystem "cl-gserver"
  :version "1.8.2"
  :author "Manfred Bergmann"
  :license "AGPL"
  :description "Actor framework featuring actors and agents for easy access to state and asynchronous operations."
  :depends-on ("alexandria"
               "bordeaux-threads"
               "lparallel"
               "cl-speedy-queue"
               "log4cl"
               "str"
               "blackbird"
               "binding-arrows"
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
                 (:file "utils")
                 (:file "actor-api")
                 (:file "dispatcher-api")
                 (:module "core"
                  :components
                  ((:file "queue")
                   (:file "message-box")
                   (:file "actor-cell")))
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
                   (:file "array-agent"))))))
  :in-order-to ((test-op (test-op "cl-gserver/tests"))))

(defsystem "cl-gserver/tests"
  :author "Manfred Bergmann"
  :license "AGPL"
  :depends-on ("cl-gserver"
               "fiveam"
               "cl-mock")
  :components ((:module "tests"
                :components
                ((:file "all-test")
                 (:file "atomic-test")
                 (:file "config-test")
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

(defsystem "cl-gserver/docs"
  :author "Manfred Bergmann"
  :license "AGPL"
  :description "Documentation for cl-gserver"
  :depends-on ("cl-gserver"
               "mgl-pax")
  :components ((:file "documentation")))

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
- add processed-p to messages so that message processing can be checked on the queued item

|#

