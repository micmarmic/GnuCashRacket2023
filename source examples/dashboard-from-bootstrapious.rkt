#lang racket
(require web-server/servlet)  ; Provides dispatch-rules.
; Provides serve/servlet and happens to provide response/full.
(require web-server/servlet-env)
(require web-server/templates)

(define TEMPLATE-DIR "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\templates\\")

(define (http-response content)  ; The 'content' parameter should be a string.
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))

(define (hello-root request)
  (http-response "Hello"))

(define (dashboard request)
  (http-response (include-template "templates/My-Dashboard.html")))

;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
               (dispatch-rules
                 [("test") hello-root]
                 [("dashboard") dashboard]
                 [else (error "There is no procedure to handle the url.")]))

(define (request-handler request)
  (printf "Request path: ~a~%" (url->string (request-uri request)))
  (dispatch request))

(define (my-start-server)
;; Start the server.
  (serve/servlet
   request-handler
   #:launch-browser? #f
   #:quit? #f
   #:listen-ip "127.0.0.1"
   #:port 8000
   #:servlet-regexp #rx""))

(my-start-server)
;(define thing "THING")
;(include-template "templates/simple-docs-chap-7.2.html")

(include-template "templates/My-Dashboard.html");
