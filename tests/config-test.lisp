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
             (retrieve-keys (config-from (prin1-to-string '(defconfig (:foo 1 :bar 2 :buzz 3))))))))

(test retrieve-value
  "Retrieves a value from a section."
  (let ((config (config-from
                 (prin1-to-string
                  '(defconfig
                    (:foo
                     (:bar 5)))))))
    (is (= 5 (retrieve-value (retrieve-section config :foo) :bar)))))

(test merge-config--no-fallback
  "Merges two configs, but fallback is nil."
  (let ((config '(:foo (:bar 1))))
    (is (equal config (merge-config config nil)))))

(test merge-config--only-fallback
  "Merges two configs, but only fallback exists."
  (let ((fallback-config '(:foo (:bar 1))))
    (is (equal fallback-config (merge-config nil fallback-config)))))

(test merge-config--overrides-in-fallback--flat
  "Merges two configs, config overrides a key in fallback."
  (let ((config '(:foo 1))
        (fallback-config '(:foo 2)))
    (is (equal '(:foo 1) (merge-config config fallback-config)))))

(test merge-config--takes-fallback--flat
  "Merges two configs, takes fallback."
  (let ((config nil)
        (fallback-config '(:foo 1)))
    (is (equal '(:foo 1) (merge-config config fallback-config)))))

(test merge-config--config+fallback--flat
  "Merges two configs, takes from both"
  (let ((config '(:bar 1))
        (fallback-config '(:foo 2 :buzz 3)))
    (is (= 1 (retrieve-value (merge-config config fallback-config) :bar)))
    (is (= 2 (retrieve-value (merge-config config fallback-config) :foo)))
    (is (= 3 (retrieve-value (merge-config config fallback-config) :buzz)))))

(test merge-config--fallback-sets-structure
  "Merges two configs, takes from both"
  (let ((config '(:foo 1))
        (fallback-config '(:foo (:bar 2))))
    (is (equal '(:bar 2) (retrieve-value (merge-config config fallback-config) :foo)))))

(test merge-config--deep
  "Merges two configs, merge deep."
  (let ((config '(:foo 1 :bar (:buzz 2)))
        (fallback-config '(:foo 2 :bar (:buzz 3 :foo2 4))))
    (is (= 1 (retrieve-value (merge-config config fallback-config) :foo)))
    (is (listp (retrieve-value (merge-config config fallback-config) :bar)))
    (is (= 2 (retrieve-value (retrieve-value (merge-config config fallback-config) :bar) :buzz)))
    (is (= 4 (retrieve-value (retrieve-value (merge-config config fallback-config) :bar) :foo2)))))

(test merge-config--config-but-no-fallback-takes-config
  "Merges two configs, when config exists as structure but not fallback then take fallback."
  (let ((config '(:foo 1 :bar (:buzz 2)))
        (fallback-config '(:foo 2)))
    (is (equal '(:buzz 2) (retrieve-value (merge-config config fallback-config) :bar)))
    (is (= 1 (retrieve-value (merge-config config fallback-config) :foo)))))

(test merge-config--config-but-no-fallback-takes-config-2
  "Merges two configs, when config exists as structure but not fallback then take fallback."
  (let ((config '(:foo 1 :bar (:buzz 2)))
        (fallback-config nil))
    (is (equal '(:buzz 2) (retrieve-value (merge-config config fallback-config) :bar)))
    (is (= 1 (retrieve-value (merge-config config fallback-config) :foo)))))
