(defsystem "clack-sse-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-sse/"
  :class :package-inferred-system
  :description "Provides tests for clack-sse."
  :source-control (:git "https://github.com/40ants/clack-sse")
  :bug-tracker "https://github.com/40ants/clack-sse/issues"
  :pathname "t"
  :depends-on ("clack-sse-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
