(defpackage :cl-gserver.logif
  (:use :cl)
  (:nicknames :lf)
  (:export #:ltrace
           #:ldebug
           #:linfo
           #:lwarn
           #:lerror
           #:config))

(in-package :cl-gserver.logif)
