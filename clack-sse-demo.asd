#-asdf3.1 (error "clack-sse-demo requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "clack-sse-demo"
  :description "A demo of Common Lisp server using Server-Sent-Events to push events from the backend to a frontend."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-sse/"
  :source-control (:git "https://github.com/40ants/clack-sse")
  :bug-tracker "https://github.com/40ants/clack-sse/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "demo"
  :depends-on ("clack-sse-demo/app"))
