(defsystem "clack-sse-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-sse/"
  :class :package-inferred-system
  :description "Provides documentation for clack-sse."
  :source-control (:git "https://github.com/40ants/clack-sse")
  :bug-tracker "https://github.com/40ants/clack-sse/issues"
  :pathname "docs"
  :depends-on ("clack-sse"
               "clack-sse-docs/index"))
