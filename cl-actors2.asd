(defsystem "cl-actors2"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("lparallel"
               "log4cl"
               "iterate"
               "trivia")
  :components ((:module "src"
                :components
                ((:file "actor"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-actors2/tests"))))

(defsystem "cl-actors2/tests"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("cl-actors2"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "actor-test"))))
  :description "Test system for cl-actors2"
  :perform (test-op (op c) (symbol-call :fiveam :run c)))
