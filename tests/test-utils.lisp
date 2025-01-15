(defpackage #:sento.test-utils
  (:use #:cl)
  (:import-from #:serapeum
                #:eval-always)
  (:import-from #:alexandria
                #:parse-body)
  (:export #:parametrized-test))
(in-package #:sento.test-utils)



(eval-always
  (defun generate-test-form (base-test-name parameter-names parameters docstring body-form)
    (let* ((test-name-str (format nil
                                  "~A-[~{~A=~S~^ ~}]"
                                  base-test-name
                                  (loop :for name :in parameter-names
                                        :for value :in parameters
                                        :appending (list name value))))
           (test-name (intern test-name-str))
           (bindings (loop :for name :in parameter-names
                           :for value :in parameters
                           :collect (list name value))))
      `(5am:test ,test-name
         ,docstring
         (let ,bindings
           ,@body-form)))))


(defmacro parametrized-test (name ((&rest parameter-names) &rest parameter-tuples) &body body)
  (multiple-value-bind (forms decls docstring)
      (parse-body body :documentation t :whole name)
    (let* ((docstring (or docstring ""))
           (body-forms (append decls forms)))
  
      (let ((tests (loop :for parameters :in parameter-tuples
                         :collect (generate-test-form name parameter-names parameters docstring body-forms))))
        `(progn
           ;; If somebody has changed parameters, we need to remove obsolte tests from the 5AM test registry.
           (loop :with prefix-to-search := ,(format nil "~A-" name)
                 :for candidate-name in (5am:test-names)
                 :for candidate-name-str := (symbol-name candidate-name)
                 :when (and (serapeum:length<= prefix-to-search candidate-name-str)
                            (string= (subseq candidate-name-str 0 (length prefix-to-search))
                                     prefix-to-search))
                   :do (5am:rem-test candidate-name))
           ,@tests)))))

