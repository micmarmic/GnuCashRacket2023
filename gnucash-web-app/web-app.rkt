#lang racket
(require web-server/servlet)  ; Provides dispatch-rules.
; Provides serve/servlet and happens to provide response/full.
(require web-server/servlet-env
         web-server/templates)

(require "gnucash-parser.rkt"
         "gnucash-objects.rkt")

(define %gnucash-data% (void)) ; global set in (start-app)

;; --------------
;; CONFIG STRINGS
;; --------------
; (define %path-data-file% "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define %path-data-file% "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")


#|
Basic webapp based on racket web-server.

The configuration strings are hard-coded below.

The data comes from gnucash-parser.rkt

Views use (require web-server/templates).

Images can be served statically using http-response-image.

|#

(define (http-response-200 content)  ; The 'content' parameter should be a string.
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))

(define (http-response-404 content)  ; The 'content' parameter should be a string.
  (response/full
    404                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))

(define (account-details request id)
  (let* ([account (send %gnucash-data% account-by-id id)]
        [account-str (send account as-string)]
        [back-url "<a href=\"/accounts\">List accounts</a>"])
    (printf "Trying to get account id '~a'. Got: ~a" id account)
    (http-response-200 (format "<p>Account as string: ~a</p>\n<p>~a</p>" account-str back-url))))

(define (account-list request)
  (define (accounts-names-urls)
    (define (name-url account)
      (list (send account get-name) (string-append "/account/" (send account get-id))))
    (map name-url (send %gnucash-data% accounts-sorted-by-name)))
  ;; TODO - current demo with clients and My-Dashboard
  (let ([accounts (accounts-names-urls)])
    (http-response-200 (include-template "templates/account-list.html"))))



(define (hello-root request)
  (http-response-200 "Hello"))

(define (dashboard request)
  (let ([clients (list (list "Smith" "Mark") (list "Simpson" "Lou"))])
    (include-template "templates/My-Dashboard.html")
    (http-response-200 (include-template "templates/My-Dashboard.html"))))

(define (generic-404 request)
  (let ([request-url (url->string (request-uri request))])
     (http-response-404 (format "Cannot dispatch URL: '~a'" request-url))))

;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
               (dispatch-rules
                [("account" (string-arg)) account-details]
                [("accounts") account-list]
                 [("") hello-root]
                 [("dashboard") dashboard]
                 [else generic-404]))

(define (request-handler request)
  (printf "Request path: ~a~%" (url->string (request-uri request)))
  (dispatch request))

(define (start-app)
  ;; load the data
  (printf "Loading data file '~a'~%" %path-data-file%) 
  (set! %gnucash-data% (import-gnucash-file %path-data-file%))
  (printf "Loading data complete.~%")
  (print-overview %gnucash-data%)
  
  (printf "Launching the web server.~%")
  ;; Start the server.
  (serve/servlet
   request-handler
   #:launch-browser? #f
   #:quit? #f
   #:listen-ip "127.0.0.1"
   #:port 8000
   #:servlet-regexp #rx""))

(start-app)

;(define thing "THING")
;(include-template "templates/simple-docs-chap-7.2.html")
