(uiop:define-package #:clack-sse-demo
  (:use #:cl)
  (:import-from #:lack)
  (:import-from #:clack)
  (:import-from #:log)
  (:import-from #:lack/util/writer-stream)
  (:nicknames #:clack-sse-demo/app))
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


(defun server-sent-events (env)
  (declare (ignore env))
  (flet ((async-handler (responder)
           (log:info "Serving events stream")

           (let* ((writer (funcall responder
                                   (list 200
                                         (list :content-type "text/event-stream"
                                               :cache-control "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"))))
                  (output-stream (lack/util/writer-stream:make-writer-stream writer))
                  (counter 0))
      
             (loop repeat 100
                   do 
                      (sleep 2)
                      ;; Without sse-server module
                      ;;(format output-stream "event:my-custom-event~%data:Hello World! ~d~%~%" (incf counter))
                      ;; With sse-server module
                      (sse-server:send-event! output-stream 
                                              "my-custom-event"
                                              (format nil "Hello World! ~d" (incf counter)))
                      (finish-output output-stream)))))
    (values #'async-handler)))


(defun index-page (env)
  (declare (ignore env))
  (log:info "Serving index")
  (list 200
        (list :content-type "text/html")
        (list *index-page*)))


(defun start-server (&key (port 8080))
  (let ((app (lack:builder
              :session
              (:static :path "/public/"
                       :root #P"/static-files/")
              (:mount "/events"
                      'server-sent-events)
              'index-page)))
    (clack:clackup app
                   :port port
                   :use-thread nil)))
