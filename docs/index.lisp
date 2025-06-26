(uiop:define-package #:clack-sse-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:clack-sse-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:clack-sse-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "clack-sse-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "clack-sse - A library to serve Server-Sent-Events from web applications based on Clack framework."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "SSE"
                                   "OUTPUT-STREAM"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "API"
                                   "URL"
                                   "URI"
                                   "RPC"
                                   "GIT"))
  (clack-sse system)
  "
[![](https://github-actions.40ants.com/40ants/clack-sse/matrix.svg?only=ci.run-tests)](https://github.com/40ants/clack-sse/actions)

![Quicklisp](http://quickdocs.org/badge/clack-sse.svg)

This library allows you to add a route to the [Clack](https://github.com/fukamachi/clack) application for serving Server-Side-Events.
SSE is an alternative to the Websocket technique, allowing to push data from the backend to the frontend.
"
  (@installation section)
  (@usage section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :clack-sse)
```
""")


(defsection @usage (:title "Usage")
  "
Here is how a server-sent-events handler could be mounted into the Clack web application.

First, we need to define a handler which will write events to the OUTPUT-STREAM:

```
(defun server-sent-events-handler (env output-stream)
  (declare (ignore env))
  (loop with counter = 0
        repeat 100
        do 
           (sleep 2)
           ;; Without sse-server module
           ;;(format output-stream \"event:my-custom-event~%data:Hello World! ~d~%~%\" (incf counter))
           ;; With sse-server module
           (sse-server:send-event! output-stream 
                                   \"my-custom-event\"
                                   (format nil \"Hello World! ~d\" (incf counter)))
           (finish-output output-stream)))
```

Then, when we'll build our app, we will use CLACK-SSE:SERVE-SSE function to create a Lack app and
`mount` middleware to call it when browser makes a request to `/events` route:

```
(defun start-server (&key (port 8080))
  (let ((app (lack:builder
              (:mount \"/events\"
                      (serve-sse 'server-sent-events-handler))
              'index-page)))
    (clack:clackup app
                   :port port
                   :use-thread nil)))
```

This example uses `sse-server` ASDF system to send event data in the correct format.
However, you can write directly to the OUTPUT-STREAM if your data is simple enough.

You will find ready to use demo application in `clack-sse-demo` ASDF system:

```
(ql:quickload :clack-sse-demo)

(clack-sse-demo:start-server :port 8080)
```
")


(defautodoc @api (:system "clack-sse"))
