(uiop:define-package #:clack-sse
  (:use #:cl)
  (:nicknames #:clack-sse/core)
  (:import-from #:log)
  (:import-from #:lack/util/writer-stream)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:export
   #:serve-sse
   #:default-on-connect))
(in-package #:clack-sse)


(defun default-on-connect (env)
  "Accepts any connection and returns headers to prevent caching.

   Here is what this function returns:

   ```
   (list 200
           (list :content-type \"text/event-stream\"
                 :cache-control \"no-store, no-cache, must-revalidate, post-check=0, pre-check=0\"))
   ```"
  (declare (ignore env))
  (list 200
        (list :content-type "text/event-stream"
              :cache-control "no-store, no-cache, must-revalidate, post-check=0, pre-check=0")))


(defun serve-sse (stream-writer &key (on-connect 'default-on-connect))
  "Returns a function suitable for accepting a connection from Clack framework and serving server-sent-events stream.

   Argument STREAM-WRITER should be a funcallable object accepting two arguments:

   - ENV argument is the same plist as usual Clack applications receive.
   - OUTPUT-STREAM is a writable stream to write events to.

   Additionally, you can provide ON-CONNECT argument. If provided, it should be a function of one argument ENV.
   This function should return a list of two items: http code and a plist of HTTP headers. You can use
   it for authentication or a session initialization. Here is what as default for ON-CONNECT argument function DEFAULT-ON-CONNECT
   is used.

   See example application in the demo/app.lisp file."
  (flet ((sse-app (env)
           (flet ((async-handler (responder)
                    (log:info "Making events stream")

                    (let* ((response (funcall on-connect))
                           (writer (funcall responder
                                            response))
                           (output-stream (lack/util/writer-stream:make-writer-stream writer)))
                      (handler-case
                          (with-log-unhandled ()
                            (funcall stream-writer
                                     env
                                     output-stream))
                        (serious-condition ()
                          ;; We just exit on any exception
                          ;; and rely on with-log-unhandled will log an error
                          ;; with the backtrace:
                          (values))))))
             (values #'async-handler))))
    (values #'sse-app)))
