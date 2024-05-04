(defpackage :sento.spawn-in-receive-test
  (:use :cl :fiveam)
  (:import-from #:miscutils
                #:assert-cond)
  (:import-from #:act
                #:*sender*
                #:reply)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :sento.spawn-in-receive-test)

(def-suite spawn-in-receive-tests
  :description "Tests doing the work in 'receive' in a separate thread"
  :in sento.tests:test-suite)

(in-suite spawn-in-receive-tests)

(defun receive-fun ()
  (lambda (msg)
    (let ((sender *sender*)) ;; capture the sender
      (case msg
        (:do-spawn
            (progn
              ;; Delegating the work could also be done via a third-party thread-pool
              ;; or a library like lparallel
              (bt2:make-thread (lambda ()
                                 (sleep 1.0)
                                 (reply :spawn-result sender)))))))))

(test spawn-in-receive
  "Spawn  a thread in `receive' which does the work.
This requires sending the response via `*sender*'"
  (let ((system (asys:make-actor-system)))
    (unwind-protect
         (let* ((actor (ac:actor-of system :receive (receive-fun)))
                (spawn-fut (act:ask actor :do-spawn)))
           (setf *sender* :foo)  ;; set something weird to *sender* to see if the capturing worked
           (is-true (assert-cond (lambda () (future:complete-p spawn-fut)) 1.5))
           (is (eq :spawn-result (future:fresult spawn-fut))))
      (ac:shutdown system))))
