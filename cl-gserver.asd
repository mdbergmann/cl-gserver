(defsystem "cl-gserver"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("lparallel"
               "log4cl"
               "iterate"
               "trivia")
  :components ((:module "src"
                :components
                ((:file "gserver"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-gserver/tests"))))

(defsystem "cl-gserver/tests"
  :author "Manfred Bergmann"
  :license ""
  :depends-on ("cl-gserver"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "gserver-test"))))
  :description "Test system for cl-gserver"
  :perform (test-op (op c) (symbol-call :fiveam :run c)))
