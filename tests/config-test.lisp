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
  (is (not (config-from nil))))

(test config-from
  "Parses config. `config-from' returns a plist where each key is a section."
  (let ((config (config-from
                 (prin1-to-string
                  '(defconfig
                    (:foo
                     (:one 1
                      :two 2)
                     :bar
                     (:three "3"
                      :four "4")))))))
    (is (not (null config)))
    (is (listp config))
    (is (equal '(:foo (:one 1 :two 2) :bar (:three "3" :four "4")) config))))

(test config-from--err
  "Parses config. Error, not correct CONFIG"
  (handler-case
      (config-from
       (prin1-to-string
        '(something)))
    (error (c)
      (is (string= "Unrecognized config!" (format nil "~a" c))))))

(test retrieve-section
  "Retrieves config section."
  (let ((config (config-from
                 (prin1-to-string
                  '(defconfig
                    (:foo
                     (:bar 5)))))))
    (is (not (null config)))
    (is (listp config))
    (is (not (null (retrieve-section config :foo))))
    (is (listp (retrieve-section config :foo)))
    (is (equal '(:bar 5) (retrieve-section config :foo)))))

(test retrieve-keys
  "Retrieves all section keys"
  (is (equal nil
             (retrieve-keys (config-from (prin1-to-string '(defconfig))))))
  (is (equal '(:foo :bar :buzz)
             (retrieve-keys (config-from (prin1-to-string '(defconfig (:foo 1 :bar 2 :buzz 3)))))))
  )

(defun run-tests ()
  (run! 'parse-empty-config)
  (run! 'config-from)
  (run! 'config-from--err)
  (run! 'retrieve-section)
  (run! 'retrieve-keys)
  )
