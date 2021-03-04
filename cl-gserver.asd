(defsystem "cl-gserver"
  :version "1.4"
  :author "Manfred Bergmann"
  :license "AGPL"
  :description "Actor framework featuring actors and agents for easy access to state and asynchronous operations."
  :depends-on ("lparallel"
               "cl-speedy-queue"
               "log4cl"
               "iterate"
               "str"
               "blackbird"
               "cl-hamt")
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
                 (:file "actor-system-test")
                 (:file "actor-tree-test")
                 (:file "spawn-in-receive-test"))))
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

#|

 (defmacro with-sym-find ()
       (let ((sym (gensym)))
         `(do-symbols (,sym)
            (if (and (boundp ,sym) (typep (symbol-value ,sym) 'act:actor))
                (progn
                  (format t "bound symbol that is actor: ~a~%" ,sym))))))

create helambdap docs:
(hlp:document (asdf:find-system :cl-gserver) :only-exported t)
(pax:update-asdf-system-html-docs cl-gserver.docs:@cl-gserver :cl-gserver :target-dir #P"docs/mgl-pax/")
|#

