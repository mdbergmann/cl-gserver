(defpackage :cl-gserver.spawn-in-receive-test
  (:use :cl :fiveam)
  (:import-from #:utils
                #:assert-cond)
  (:import-from #:act-cell
                #:*sender*)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-gserver.spawn-in-receive-test)

(def-suite spawn-in-receive-tests
  :description "Tests doing the work in 'receive' in a separate thread"
  :in cl-gserver.tests:test-suite)

(in-suite spawn-in-receive-tests)

(defun actor-creator ()
  (lambda ()
    (act:make-actor
     (lambda (self msg state)
       (declare (ignore self))
       (let ((sender *sender*))  ;; capture the sender
         (case msg
           (:do-spawn (progn
                        ;; Delegating the work could also be done via a third-party thread-pool
                        ;; or a library like lparallel
                        (bt:make-thread (lambda ()
                                          (sleep 0.5)
                                          (act:tell sender :spawn-result)))
                        ;; we must returns with a `cons'.
                        ;; specify `:no-reply' to defer responding to the caller.
                        (cons :no-reply state)))
           (t (cons :no-spawn-result state))))))))

(test spawn-in-receive
  "Spawn  a thread in `receive' which does the work.
This requires to return `:no-reply' in `receive' and sending the response via `*sender*'"

  (let ((system (asys:make-actor-system)))
    (unwind-protect
         (let* ((actor (ac:actor-of system (actor-creator)))
                (no-spawn-fut (act:ask actor :no-spawn))
                (spawn-fut (act:ask actor :do-spawn)))
           (is-true (assert-cond (lambda () (future:complete-p no-spawn-fut)) 1))
           (is (eq :no-spawn-result (future:get-result no-spawn-fut)))

           (is-true (assert-cond (lambda () (future:complete-p spawn-fut)) 1))
           (is (eq :spawn-result (future:get-result spawn-fut))))
      (ac:shutdown system))))

(defun run-tests ()
  (run! 'spawn-in-receive))
