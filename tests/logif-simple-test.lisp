(defpackage :cl-gserver.logif-simple-test
  (:use :cl :fiveam :cl-gserver.logif)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.logif-simple-test)

(def-suite logif-simple-tests
  :description "Log interface simple test"
  :in cl-gserver.tests:test-suite)

(in-suite logif-simple-tests)

(def-fixture with-stream ()
  (let ((stream (make-string-output-stream)))
    (lf:config :level :trace)
    (setf lf::*log-stream* stream)
    (&body)))

(test log-config
  (lf:config :level :trace)
  (is (equal '(:trace . 1) lf::*level*))
  (lf:config :level :error)
  (is (equal '(:error . 5) lf::*level*)))

(test log-on-level--equal
  (with-fixture with-stream ()
    (lf:ltrace "foo")
    (is (str:starts-with-p "<TRACE> [CL-GSERVER.LOGIF-SIMPLE-TEST] foo" (get-output-stream-string stream)))))

(test log-on-level--lower
  (with-fixture with-stream ()
    (lf:config :level :info)
    (lf:ldebug "foo")
    (is (str:emptyp (get-output-stream-string stream)))))

(test log-on-level--higher
  (with-fixture with-stream ()
    (lf:ldebug "foo")
    (is (str:starts-with-p "<DEBUG> [CL-GSERVER.LOGIF-SIMPLE-TEST] foo" (get-output-stream-string stream)))))

(test log-on-level--changing-config
  (with-fixture with-stream ()
    (lf:ltrace "foo")
    (is (str:starts-with-p "<TRACE> [CL-GSERVER.LOGIF-SIMPLE-TEST] foo" (get-output-stream-string stream))))
  (with-fixture with-stream ()
    (lf:config :level :warn)
    (lf:ldebug "foo")
    (is (str:emptyp (get-output-stream-string stream)))))

(test log-on-level--higher
  (with-fixture with-stream ()
    (lf:ldebug "foo")
    (is (str:starts-with-p "<DEBUG> [CL-GSERVER.LOGIF-SIMPLE-TEST] foo" (get-output-stream-string stream)))))

(test log-object
  (with-fixture with-stream ()
    (lf:ldebug '())
    (is (str:starts-with-p "<DEBUG> [CL-GSERVER.LOGIF-SIMPLE-TEST] ()"
                           (get-output-stream-string stream)))
    (lf:ldebug '(1 2 3))
    (is (str:starts-with-p "<DEBUG> [CL-GSERVER.LOGIF-SIMPLE-TEST] (1 2 3)"
                           (get-output-stream-string stream)))
    (lf:ldebug #(1 2 3))
    (is (str:starts-with-p "<DEBUG> [CL-GSERVER.LOGIF-SIMPLE-TEST] #(1 2 3)"
                           (get-output-stream-string stream)))
    (lf:ldebug 10)
    (is (str:starts-with-p "<DEBUG> [CL-GSERVER.LOGIF-SIMPLE-TEST] 10"
                           (get-output-stream-string stream)))))
