(defpackage :cl-gserver.utils
  (:nicknames :utils)
  (:use :cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:mkstr
           #:assert-cond
           #:filter))

(in-package :cl-gserver.utils)

(defun mkstr (&rest args)
  "Converts all parameters to string and concatenates them."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun assert-cond (assert-fun max-time)
  (do ((wait-time 0.02 (+ wait-time 0.02))
       (fun-result nil (funcall assert-fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep 0.02))))

(defmacro filter (fun list)
  (with-gensyms (x)
  `(mapcan (lambda (,x) (if (funcall ,fun ,x) (list ,x))) ,list)))
