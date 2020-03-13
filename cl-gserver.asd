(defsystem "cl-gserver"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("lparallel"
               "log4cl"
               "iterate"
               "trivia")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "gserver")
                 (:file "agent"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-gserver/tests"))))

(defsystem "cl-gserver/tests"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("cl-gserver"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "all-test")
                 (:file "utils-test")
                 (:file "gserver-test")
                 (:file "gserver-mp-test")
                 (:file "agent-test"))))
  :description "Test system for cl-gserver"
  :perform (test-op (op c) (symbol-call :fiveam :run! 'cl-gserver.tests)))

;; add to asdf:*central-registry* is not done
;; (push #P"~/Development/MySources/cl-gserver/" asdf:*central-registry*)
;;
;; load system
;; (asdf:load-system "cl-gserver")
;;
;; test system
;; (asdf:test-system "cl-gserver/tests")
