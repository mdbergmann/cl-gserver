(defsystem "sento"
  :version "2.2.0"
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
                 (:file "stash-test")
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
               "mgl-pax/full")
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

OK - *self*, *state*, *sender* should be in act package
=> - add !, ? as functions.
- maybe add 'reply' for 'ask'
- shutdown sys should be in actor-system to be symetric.
- rename all-actors to all-actors-in
- rename utils package
- move cons-list from lparallel
- investigate:  <WARN> [17:29:22] sento.actor-cell actor-cell.lisp () - actor-26696: ask-s timeout: A timeout set to 0.5 seconds occurred. Cause: NIL 

Sento 3 changes:

- no implicit sending to *sender* on 'ask'. Must be done explicitly using 'tell'.
- removed required 'cons' return on 'receive' function.
- removed 'self' and 'state' in 'receive function. Now exists *self* and *state*.

|#

