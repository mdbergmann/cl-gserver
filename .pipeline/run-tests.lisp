;;;; Offline test runner for the AI pipeline's test stage.
;;;;
;;;; Run as:  sbcl --script .pipeline/run-tests.lisp
;;;;
;;;; Kept as a script (rather than an inline --eval string in .pipeline.yml) so
;;;; there is no shell-quoting to mangle. Dependencies resolve from the vendored
;;;; systems/ tree via CL_SOURCE_REGISTRY, set in the agent image; no network is
;;;; needed, which is what lets the test stage run with --network none.
;;;;
;;;; Exits non-zero if any test fails (or if loading errors) so the pipeline
;;;; sees a real pass/fail — asdf:test-system alone would not signal failure.

(require :asdf)

(asdf:load-system :sento/tests)

(unless (fiveam:run! 'sento.tests:test-suite)
  (uiop:quit :unix-status 1))
