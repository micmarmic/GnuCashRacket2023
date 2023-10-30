#lang racket
(require web-server/servlet)  ; Provides dispatch-rules.
; Provides serve/servlet and happens to provide response/full.
(require web-server/servlet-env)

(define (http-response content)  ; The 'content' parameter should be a string.
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))

(define (first-page request)
  (http-response "<h1>This is the first page!</h1>"))

(define (second-page request)
  (http-response "<h1>This is the second page!</h1>"))

(define (show-time-page request)
  (http-response (number->string (current-seconds))))

(define (greeting-page request human-name)  ; Notice the additional parameter.
  (http-response (string-append "Hello " human-name "!")))

;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
               (dispatch-rules
                 [("first-page") first-page]
                 [("second-page") second-page]
                 [("time") show-time-page]
                 [("hello" (string-arg)) greeting-page]  ; Notice this line.
                 [else (error "There is no procedure to handle the url.")]))

(define (request-handler request)
  (dispatch request))

;; Start the server.
(serve/servlet
  request-handler
  #:launch-browser? #f
  #:quit? #f
  #:listen-ip "127.0.0.1"
  #:port 8000
  #:servlet-regexp #rx"")