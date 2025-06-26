<a id="x-28CLACK-SSE-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# clack-sse - A library to serve Server-Sent-Events from web applications based on Clack framework.

<a id="clack-sse-asdf-system-details"></a>

## CLACK-SSE ASDF System Details

* Description: A library to serve Server-Sent-Events from web applications based on Clack framework.
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/clack-sse/][3947]
* Bug tracker: [https://github.com/40ants/clack-sse/issues][f9a3]
* Source control: [GIT][66c6]
* Depends on: [lack-util-writer-stream][2580], [log4cl][7f8b], [log4cl-extras][691c]

[![](https://github-actions.40ants.com/40ants/clack-sse/matrix.svg?only=ci.run-tests)][c325]

![](http://quickdocs.org/badge/clack-sse.svg)

This library allows you to add a route to the [Clack][75f7] application for serving Server-Side-Events.
`SSE` is an alternative to the Websocket technique, allowing to push data from the backend to the frontend.

<a id="x-28CLACK-SSE-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :clack-sse)
```
<a id="x-28CLACK-SSE-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

Here is how a server-sent-events handler could be mounted into the Clack web application.

First, we need to define a handler which will write events to the `OUTPUT-STREAM`:

```
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
```
Then, when we'll build our app, we will use [`clack-sse:serve-sse`][a9ba] function to create a Lack app and
`mount` middleware to call it when browser makes a request to `/events` route:

```
(defun start-server (&key (port 8080))
  (let ((app (lack:builder
              (:mount "/events"
                      (serve-sse 'server-sent-events-handler))
              'index-page)))
    (clack:clackup app
                   :port port
                   :use-thread nil)))
```
This example uses `sse-server` `ASDF` system to send event data in the correct format.
However, you can write directly to the `OUTPUT-STREAM` if your data is simple enough.

You will find ready to use demo application in `clack-sse-demo` `ASDF` system:

```
(ql:quickload :clack-sse-demo)

(clack-sse-demo:start-server :port 8080)
```
<a id="x-28CLACK-SSE-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28CLACK-SSE-DOCS-2FINDEX-3A-3A-40CLACK-SSE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CLACK-SSE

<a id="x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-22CLACK-SSE-22-29-20PACKAGE-29"></a>

#### [package](215b) `clack-sse`

<a id="x-28CLACK-SSE-DOCS-2FINDEX-3A-3A-7C-40CLACK-SSE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CLACK-SSE-3ADEFAULT-ON-CONNECT-20FUNCTION-29"></a>

##### [function](1108) `clack-sse:default-on-connect` env

<a id="x-28CLACK-SSE-3ASERVE-SSE-20FUNCTION-29"></a>

##### [function](6fb6) `clack-sse:serve-sse` stream-writer &key (on-connect 'default-on-connect)

Returns a function suitable for accepting a connection from Clack framework and serving server-sent-events stream.

Argument `STREAM-WRITER` should be a funcallable object accepting two arguments:

* `ENV` argument is the same plist as usual Clack applications receive.
* `OUTPUT-STREAM` is a writable stream to write events to.

Additionally, you can provide `ON-CONNECT` argument. If provided, it should be a function of one argument `ENV`.
This function should return a list of two items: http code and a plist of `HTTP` headers. You can use
it for authentication or a session initialization. Here is what default `ON-CONNECT` argument returns:

```
(list 200
        (list :content-type "text/event-stream"
              :cache-control "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"))
```
See example in the demo/app.lisp file.


[3947]: https://40ants.com/clack-sse/
[a9ba]: https://40ants.com/clack-sse/#x-28CLACK-SSE-3ASERVE-SSE-20FUNCTION-29
[66c6]: https://github.com/40ants/clack-sse
[c325]: https://github.com/40ants/clack-sse/actions
[215b]: https://github.com/40ants/clack-sse/blob/59fa69069989c122e6ae91d068c89aa8ce95e192/src/core.lisp#L1
[1108]: https://github.com/40ants/clack-sse/blob/59fa69069989c122e6ae91d068c89aa8ce95e192/src/core.lisp#L14
[6fb6]: https://github.com/40ants/clack-sse/blob/59fa69069989c122e6ae91d068c89aa8ce95e192/src/core.lisp#L21
[f9a3]: https://github.com/40ants/clack-sse/issues
[75f7]: https://github.com/fukamachi/clack
[2580]: https://quickdocs.org/lack-util-writer-stream
[7f8b]: https://quickdocs.org/log4cl
[691c]: https://quickdocs.org/log4cl-extras

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
