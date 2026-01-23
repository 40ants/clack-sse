(uiop:define-package #:clack-sse-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:clack-sse-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.1.1 2026-01-23
         "* Fixed issue with calling on-connect function during connect initialization.")
  (0.1.0 2025-06-27
         "* Initial version."))
