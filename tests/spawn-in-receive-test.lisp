(defpackage :sento.spawn-in-receive-test
  (:use :cl :fiveam)
  (:import-from #:miscutils
                #:assert-cond)
  (:import-from #:act-cell
                #:*sender*)
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
        (:do-spawn (progn
                     ;; Delegating the work could also be done via a third-party thread-pool
                     ;; or a library like lparallel
                     (bt:make-thread (lambda ()
                                       (sleep 0.5)
                                       (act:tell sender :spawn-result)))))))))

(test spawn-in-receive
  "Spawn  a thread in `receive' which does the work.
This requires to return `:no-reply' in `receive' and sending the response via `*sender*'"
  (let ((system (asys:make-actor-system)))
    (unwind-protect
         (let* ((actor (ac:actor-of system :receive (receive-fun)))
                (spawn-fut (act:ask actor :do-spawn)))
           (is-true (assert-cond (lambda () (future:complete-p spawn-fut)) 1))
           (is (eq :spawn-result (future:fresult spawn-fut))))
      (ac:shutdown system))))
