#-asdf3.1 (error "clack-sse requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "clack-sse"
  :description "A library to serve Server-Sent-Events from web applications based on Clack framework."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-sse/"
  :source-control (:git "https://github.com/40ants/clack-sse")
  :bug-tracker "https://github.com/40ants/clack-sse/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("clack-sse/core")
  :in-order-to ((test-op (test-op "clack-sse-tests"))))


(asdf:register-system-packages "log4cl" '("LOG"))
(asdf:register-system-packages "lack-util-writer-stream" '("LACK/UTIL/WRITER-STREAM"))
