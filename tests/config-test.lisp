(defpackage :cl-gserver.config-test
  (:use :cl :fiveam :cl-gserver.config)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.config-test)

(def-suite config-tests
  :description "Tests for config parsing"
  :in cl-gserver.tests:test-suite)

(in-suite config-tests)

(test parse-empty-config
  "Parses empty config"
  (is (not (parse-config nil))))

(test parse-config
  "Parses config. `parse-config' returns a plist where each key is a section."
  (let ((config (parse-config
                 (prin1-to-string
                  '(config
                    (:foo
                     (:one 1
                      :two 2)
                     :bar
                     (:three "3"
                      :four "4")))))))
    (is (not (null config)))
    (is (listp config))
    (is (equal '(:foo (:one 1 :two 2) :bar (:three "3" :four "4")) config))))

(test parse-config--err
  "Parses config. Error, not correct CONFIG"
  (handler-case
      (parse-config
       (prin1-to-string
        '(something)))
    (error (c)
      (is (string= "Unrecognized config!" (format nil "~a" c))))))

(test retrieve-dispatcher-section
  "Retrieves config section dispatchers"
  (let ((config (parse-config
                 (prin1-to-string
                  '(config
                    (:dispatchers
                     (:num-dispatchers 5)))))))
    (is (not (null config)))
    (is (listp config))
    (is (not (null (get-section config :dispatchers))))
    (is (listp (get-section config :dispatchers)))
    (is (equal '(:num-dispatchers 5) (get-section config :dispatchers)))))

(test retrieve-sections
  "Retrieves all section keys"
  (is (equal nil
             (retrieve-sections (parse-config (prin1-to-string '(config))))))
  (is (equal '(:foo :bar :buzz)
             (retrieve-sections (parse-config (prin1-to-string '(config (:foo 1 :bar 2 :buzz 3)))))))
  )

(defun run-tests ()
  (run! 'parse-empty-config)
  (run! 'parse-config)
  (run! 'parse-config--err)
  (run! 'retrieve-dispatcher-section)
  (run! 'retrieve-sections)
  )
