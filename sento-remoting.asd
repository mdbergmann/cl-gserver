(defsystem "sento-remoting"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :description "Remoting extension for Sento actor framework. Enables actors to communicate across the network over TLS 1.3."
  :depends-on ("sento"
               "flexi-streams"
               "log4cl")
  :components ((:module "src"
                :serial t
                :components
                ((:module "remoting"
                  :serial t
                  :components
                  ((:file "remoting-api")
                   (:file "serialization")
                   (:file "envelope"))))))
  :in-order-to ((test-op (test-op "sento-remoting/tests"))))

(defsystem "sento-remoting/tests"
  :author "Manfred Bergmann"
  :depends-on ("sento-remoting"
               "fiveam"
               "cl-mock")
  :components ((:module "tests"
                :serial t
                :components
                ((:module "remoting"
                  :serial t
                  :components
                  ((:file "all-remoting-test")
                   (:file "conditions-test")
                   (:file "serialization-test")
                   (:file "envelope-test"))))))
  :description "Test system for sento-remoting"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:remoting-test-suite
                                                           '#:sento.remoting.tests))))
