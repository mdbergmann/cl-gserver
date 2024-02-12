(defsystem "sento"
  :version "3.1.1"
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
               "lparallel"
               "cl-mock")
  :components ((:module "tests"
                :components
                ((:file "all-test")
                 (:file "miscutils-test")
                 (:file "atomic-test")
                 (:file "config-test")
                 (:file "wheel-timer-test")
                 (:file "timeutils-test")
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
;; lparallels high speec cons-queue
;; this is a separate system in order to reduce dependencies
;; for 'normal' use the jpl-queues are fully sufficient
;; --------------------------------

(defsystem "sento/high-speed-queue"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :description "High speed unbounded queue based on lparallel's cons-queue."
  :depends-on ("sento"
               "lparallel"
               )
  :components ((:module "src"
                :serial t
                :components
                ((:module "queue"
                  :components
                  ((:file "queue-hsq"))))))
  :in-order-to ((test-op (test-op "sento-high-speed-queue/tests"))))

(defsystem "sento/high-speed-queue-tests"
  :author "Manfred Bergmann"
  :depends-on ("sento/high-speed-queue"
               "fiveam"
               "cl-mock")
  :components ((:module "tests"
                :components
                ((:file "all-test")
                 (:file "actor-cell-test")
                 (:file "actor-mp-test")
                 )))
  :description "Test system for sento"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:sento.tests))))

;; load system
;; (asdf:load-system "sento")
;;
;; test system
;; (asdf:test-system "sento/tests")
;;
;; (hlp:document (asdf:find-system :sento) :only-exported t)
;; (pax:update-asdf-system-html-docs sento.docs::@sento :sento :target-dir #P"~/docs/")

#|

TODOs:

OK - *self*, *state*, *sender* should be in act package
OK - add !, ? as functions.
OK - shutdown sys should be in actor-system to be symetric.
  ==> stays as is. shutdown method is in asys but is ac protocol.
OK - rename utils package
NO - compose actor of actor-cell
  ==> core functionality that shouldn't be really used is prrovided by ACT-CELL, like (STOP, NAME, STATE, RUNNING-P). Instead the AC provided functionality should be used.
OK - add 'reply' macro for responding in side receive, with sender
OK - move cons-list from lparallel
  ==> using jpl-queues and con-squeue as a separate system.
OK - write new readme with new features
OK - migration guide
OK - potential problem with reply after async operation.

Sento 3 changes:

OK - no implicit sending to *sender* on 'ask'. Must be done explicitly using 'tell'.
OK - removed required 'cons' return on 'receive' function.
OK - removed 'self' and 'state' in 'receive function. Now exists *self* and *state*.
OK - added reply
OK - cons-queue as separate 'high-speed' system

Sento 3.1 todos:

OK - check COUNTER-MP-UNBOUNDED and BOUNDED tests for LW 8
OK - create documentation MGL-PAX
OK - tag version
OK - upload

|#

