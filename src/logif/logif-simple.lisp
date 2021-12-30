(in-package :cl-gserver.logif)

(defparameter *levels* '((:trace . 1)
                         (:debug . 2)
                         (:info . 3)
                         (:warn . 4)
                         (:error . 5)))

(defparameter *log-stream* *standard-output*
  "The output stream")

(defun %find-level (level)
  (find-if (lambda (x)
             (eq (car x) level))
           *levels*))

(defparameter *level* (%find-level :warn)
  "The current log level")

(defun config (&key level)
  "Config logging.

level: log config, one of :trace, :debug, :info, :warn, :error

I.e.:

```
(lf:config :level :debug)
```
"
  (let ((new-level (%find-level level)))
    (assert (not (null new-level)) nil "Unknown log level!")
    (setf *level* new-level)))

(defmacro do-log (level message rest)
  (let ((level-num (gensym)))
    (setf level-num (cdr (%find-level level)))
    `(let ((curr-level-num (cdr *level*)))
       (when (<= curr-level-num ,level-num)
         (let ((formatted-message (typecase ,message
                                    (string (format nil ,message ,@rest))
                                    (t (format nil "~:a" ,message)))))
           (format *log-stream* "<~a> [~8a] ~a~%" ,level (package-name *package*) formatted-message))))))

(defmacro ltrace (message &rest rest)
  `(do-log :trace ,message ,rest))

(defmacro ldebug (message &rest rest)
  `(do-log :debug ,message ,rest))

(defmacro linfo (message &rest rest)
  `(do-log :info ,message ,rest))

(defmacro lwarn (message &rest rest)
  `(do-log :warn ,message ,rest))

(defmacro lerror (message &rest rest)
  `(do-log :error ,message ,rest))
