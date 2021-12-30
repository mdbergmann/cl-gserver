(in-package :cl-gserver.logif)

(shadowing-import '(log:debug
                    log:trace
                    log:info
                    log:warn
                    log:error))

(defmacro ltrace (message &rest rest)
  `(log:trace ,message ,@rest))

(defmacro ldebug (message &rest rest)
  `(log:debug ,message ,@rest))

(defmacro linfo (message &rest rest)
  `(log:info ,message ,@rest))

(defmacro lwarn (message &rest rest)
  `(log:warn ,message ,@rest))

(defmacro lerror (message &rest rest)
  `(log:error ,message ,@rest))
