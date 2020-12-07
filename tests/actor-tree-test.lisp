(defpackage :cl-gserver.actor-tree-test
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.actor-tree-test)

(def-suite actor-tree-tests
  :description "Tests on actor trees."
  :in cl-gserver.tests:test-suite)

(in-suite actor-tree-tests)

(test actor-tree
  "Paths, etc. in a tree."

  (let* ((asystem (asys:make-actor-system :shared-dispatcher-workers 0))
         (root (ac:actor-of asystem
                            (lambda () (act:make-actor
                                   (lambda (self msg state)
                                     (declare (ignore self msg state)))
                                   :name "1"))))
         (first (ac:actor-of (act:context root)
                             (lambda () (act:make-actor
                                    (lambda (self msg state)
                                      (declare (ignore self msg state)))
                                    :name "2"))))
         (second (ac:actor-of (act:context first)
                              (lambda () (act:make-actor
                                     (lambda (self msg state)
                                       (declare (ignore self msg state)))
                                     :name "3"))))
         (third (ac:actor-of (act:context second)
                             (lambda () (act:make-actor
                                    (lambda (self msg state)
                                      (declare (ignore self msg state)))
                                    :name "4")))))

    (is (string= "/user/1" (act:path root)))
    (is (string= "/user/1/2" (act:path first)))
    (is (string= "/user/1/2/3" (act:path second)))
    (is (string= "/user/1/2/3/4" (act:path third)))))
