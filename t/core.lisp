(uiop:define-package #:clack-sse-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:clack-sse-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
