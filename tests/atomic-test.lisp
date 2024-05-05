(defpackage :sento.atomic-test
  (:use :cl :fiveam :sento.atomic)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.atomic-test)

(def-suite atomic-tests
  :description "Tests for atomic."
  :in sento.tests:test-suite)

(in-suite atomic-tests)

(test make-atomic-reference
  "Tests making an atomic reference"
  (is (make-atomic-reference :value '())))

(test swap-reference
  "Swap a reference."
  (let* ((ref (make-atomic-reference :value '(0)))
         (fn1 #'append)
         (args (list 1 2))
         (fn2 (lambda (lst) (apply #'+ lst))))
    (is (equal '(0 1 2) (atomic-swap ref fn1 args))) ; test with rest args
    (is (equal '(0 1 2) (atomic-get ref)))
    (is (= 3 (atomic-swap ref fn2))) ; test without rest args
    (is (= 3 (atomic-get ref)))))

(test make-atomic-integer
  "Tests making an atomic integer"
  (is (make-atomic-integer :value 5)))

(test swap-integer
  "Swap an intreger value."
  (let* ((value (make-atomic-integer :value 5))
         (fn1 #'+)
         (fn2 #'1+))
    (is (= 8 (atomic-swap value fn1 1 2)))
    (is (= 8 (atomic-get value)))
    (is (= 9 (atomic-swap value fn2)))
    (is (= 9 (atomic-get value)))))

#+abcl
(test abcl-atomic-swap-integer
  "ABCL atomic swap integer (as well as atomic-cas) test in threads."
  (let* ((n 100)
         (aint (make-atomic-integer :value 0))
         (threads (loop repeat n
                        collect (bt2:make-thread (lambda ()
                                                   (atomic-swap aint (lambda (int) (+ int 1000)))
                                                   (atomic-swap aint (lambda (int) (- int 10000)))
                                                   (atomic-swap aint (lambda (int) (+ int 10000)))
                                                   (atomic-swap aint (lambda (int) (- int 1000))))))))
    (mapc #'bt2:join-thread threads)
    (is (= 0 (atomic-get aint)))))

#+abcl
(test abcl-atomic-swap-reference
  "ABCL atomic swap reference (as well as atomic-cas) test in threads."
  (let* ((n 100)
         (a-ref (make-atomic-reference :value nil))
         (threads (loop repeat n
                        collect (bt2:make-thread (lambda ()
                                                   (atomic-swap a-ref (lambda (ref) (push 0 ref)))
                                                   (atomic-swap a-ref (lambda (ref) (pop ref) nil))
                                                   (atomic-swap a-ref (lambda (ref) (push 1 ref)))
                                                   (atomic-swap a-ref (lambda (ref) (pop ref) nil)))))))
    (mapc #'bt2:join-thread threads)
    (is (eql nil (atomic-get a-ref)))))

#+abcl
(test atomic-incf
  "Basic tests of increasing an atomic-integer object."
  (let ((aint (make-atomic-integer :value 0)))
    (is (= 0 (atomic::atomic-incf aint)))
    (is (= 1 (atomic-get aint)))
    (is (= 1 (atomic::atomic-incf aint 10)))
    (is (= 11 (atomic-get aint)))
    (is (= 11 (atomic::atomic-incf aint -10)))
    (is (= 1 (atomic-get aint)))))

#+abcl
(test atomic-incf-threads
  "Test increasing an atomic-integer object in threads."
  (let* ((n 100)
         (aint (make-atomic-integer :value 0))
         (threads (loop repeat n
                        collect (bt2:make-thread (lambda ()
                                                   (atomic::atomic-incf aint 100)
                                                   (atomic::atomic-incf aint -1000)
                                                   (atomic::atomic-incf aint +1000)
                                                   (atomic::atomic-incf aint -100))))))
    (mapc #'bt2:join-thread threads)
    (is (= 0 (atomic-get aint)))))
