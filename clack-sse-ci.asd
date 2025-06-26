(defsystem "clack-sse-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-sse/"
  :class :package-inferred-system
  :description "Provides CI settings for clack-sse."
  :source-control (:git "https://github.com/40ants/clack-sse")
  :bug-tracker "https://github.com/40ants/clack-sse/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "clack-sse-ci/ci"))
