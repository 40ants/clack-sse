(uiop:define-package #:clack-sse-demo
  (:use #:cl)
  (:import-from #:lack)
  (:import-from #:clack)
  (:import-from #:log)
  (:import-from #:lack/util/writer-stream)
  (:import-from #:clack-sse
                #:serve-sse)
  (:nicknames #:clack-sse-demo/app)
  (:export #:start-server))
(in-package #:clack-sse-demo)


(defparameter *index-page*
  "<!DOCTYPE html>
<html>
<head>
    <meta charset=\"utf-8\" />
    <meta name=\"viewport\" content=\"width=device-width, height=device-height\" />
    <title>Server-Sent Events Demo</title>
    <style type=\"text/css\">
        body {
            font-family: 'Open Sans', sans-serif;
        }
    </style>
</head>
<body>

    <h1>Server-Sent Events Demo</h1>

    <ul></ul>

    <script>
        (function() { \"use strict\";
            var ul = document.querySelector('ul');
            var es = new EventSource('/events');
            function li(text) {
                var li = document.createElement('li');
                li.innerText = text;
                ul.appendChild(li);
            }
            es.addEventListener('open', function() {
                li('Server connected :)');
            });
            es.addEventListener('my-custom-event', function(event) {
                li(event.data);
            });
            es.addEventListener('error', function() {
                li('Server unavailable :(');
            });
        })();
    </script>
</body>
</html>
")


(defun server-sent-events-handler (env output-stream)
  (declare (ignore env))
  (loop with counter = 0
        repeat 100
        do 
           (sleep 2)
           ;; Without sse-server module
           ;;(format output-stream "event:my-custom-event~%data:Hello World! ~d~%~%" (incf counter))
           ;; With sse-server module
           (sse-server:send-event! output-stream 
                                   "my-custom-event"
                                   (format nil "Hello World! ~d" (incf counter)))
           (finish-output output-stream)))


(defun index-page (env)
  (declare (ignore env))
  (log:info "Serving index")
  (list 200
        (list :content-type "text/html")
        (list *index-page*)))


(defun start-server (&key (port 8080))
  "Starts a demo server showing how does SSE demo work."
  (let ((app (lack:builder
              (:mount "/events"
                      (serve-sse 'server-sent-events-handler))
              'index-page)))
    (clack:clackup app
                   :port port
                   :use-thread nil)))
