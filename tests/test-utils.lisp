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
  "Generates a separate tests for each parameter combination.

   - NAME is the prefix for all tests in the group. The rest of each test name consists of parameters and values.
   - PARAMETER-NAMES should be a list of symbolic names of variables to be bound during BODY execution.
   - PARAMETER-TUPLES should be a list of lists of values to be bound to variables given in PARAMETER-NAMES.

   Example:

   (parametrized-test bt-box-test
       ((withreply-p timeout)
        (nil         nil)
        (t           1)
        (t           nil))

     (do-something with-reply-p timeout))

   This form will be expanded to the code which will remove all 5AM tests starting with BT-BOX-TEST-
   and then will create 3 tests like this one:


   (test |BT-BOX-TEST-[WITHREPLY-P=T TIMEOUT=1]|
      (let ((withreply-p t) (timeout 1))
        (do-something with-reply-p timeout)))

   As you can see, this test binds WITHREPLY-P and TIMEOUT variables to a values given in the second row of PARAMETER-TUPLES.

   Name of each test will include parameter variables for this test. This way it will be easy to tell which parameter combination
   fails.
"
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

