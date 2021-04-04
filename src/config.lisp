(defpackage :cl-gserver.config
  (:use :cl)
  (:nicknames :config)
  (:export #:parse-config
           #:get-section
           #:retrieve-sections))

(in-package :cl-gserver.config)

(defun parse-config (config-string)
  "Parses the given config-string, represented by common lisp s-expressions.
Returns a parsed (as in run through `cl:read`) config."
  (when config-string
    (let* ((stream (make-string-input-stream config-string))
           (config (read stream)))
      (if (string= "CONFIG" (symbol-name (car config)))
          (cadr config)
          (error "Unrecognized config!")))))

(defun get-section (config section)
  "Retrieves the given named section which should be a (global) `symbol` (a key).
A section usually is a plist with additional configs or sub sections.
This function looks only in the root hierarchy of the given config."
  (getf config section))

(defun retrieve-sections (config)
  "Retrieves all section keys"
  (loop :for (sec nil) :on config :by #'cddr
        :collect sec))
