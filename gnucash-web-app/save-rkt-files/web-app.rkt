#lang racket
(require web-server/servlet)  ; Provides dispatch-rules.
; Provides serve/servlet and happens to provide response/full.
(require web-server/servlet-env
         web-server/templates)

(require "gnucash-parser.rkt"
         "gnucash-objects.rkt"
         "views.rkt")

(define %gnucash-data% (void)) ; global set in (start-app)
(define %base-template-path% "templates/base-template.html")

;; ----------------
;;  CONFIG STRINGS
;; ----------------
(define %path-data-file% "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
;(define %path-data-file% "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")

(struct parent (name children))
(struct child (name))
(define parents
  (list
   (parent "Josie"(list (child "Bill") (child "Linus")))
   (parent "Jack"(list (child "Sam") (child "Shelly")))))
#|
(for ([parent parents])
  (printf "~a~%" (parent-name parent))
  (for ([child (parent-children parent)])
    (printf "   ~a~%" (child-name child))))
|#      


#|
Basic webapp based on racket web-server.

The configuration strings are hard-coded below.

The data comes from gnucash-parser.rkt

Views use (require web-server/templates).

Images can be served statically using http-response-image.



(define (response-200-base-template page-title main-content-heading main-content)
  (let ([html (include-template "templates/base-template.html")])
    (http-response-200 html)))


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
  (let* ([page-title "Accounts | GnuCash App"]
        [main-content-heading "Accounts"]
        [accounts (accounts-names-urls)]
        [main-content (include-template "templates/account-list.html")])
    (response-200-base-template page-title main-content-heading main-content)))

|#

#|
;; don't know how to nest a loop in a template; do it here
(define (ul-children children)
  (if (null? children)
      "<p><strong>NO PARENTS</strong></p>"
      (let loop ([chlds children] [html ""])
        (if (null? chlds)
            html
            (loop
             (rest chlds)
             (string-append html "\n"
                           (child-name (first chlds))))))))
|#


                            
#|
(define (ul-parents)
  (if (null? arg-parents)
      "<p><strong>NO PARENTS</strong></p>"
      (let loop ([parents arg-parents] [html ""])
        (if (null? parents)
            html
            (loop
             (rest parents)
             (string-append html
                            (format "<h1>~a</h1>~%" (parent-name (first parents)))
                            (ul-children (parent-children (first parents)))))))))
|#

(define (test-dispatch request text)
  (http-response-200 text))

#|
(define (dashboard request)
  (let ([clients (list (list "Smith" "Mark") (list "Simpson" "Lou"))])
    (include-template "templates/My-Dashboard.html")
    (http-response-200 (include-template "templates/My-Dashboard.html"))))
|#


;; -----------------------------------
;; URL routing table (URL dispatcher).
;; -----------------------------------

(define (get-url request)
  (url->string (request-uri request)))

(define-values (dispatch generate-url)
               (dispatch-rules
                ;; id page-number split-flag (s1 display splits, else just trans)
                [("account" (string-arg) (integer-arg) (string-arg))
                 (lambda (request account-id page-num split-flag)
                   (ledger-view %gnucash-data% account-id request page-num split-flag (get-url request)))]
                ;[("account" (string-arg)) (lambda (request account-id) (ledger-view %gnucash-data% account-id request 0))]
                ;[("account" (string-arg)) (lambda (request string-arg) (account-details %gnucash-data% request string-arg))]
                [("accounts") (lambda (request) (account-list %gnucash-data% request))]
                [("parents") parents-demo]
                [("") (lambda (request) (account-list %gnucash-data% request))]
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
  (printf "Found ~a accounts.~%" (send %gnucash-data% num-accounts))
  ; REDO account-based, not repo-based (print-overview %gnucash-data%)
  (send %gnucash-data% clear-all-transactions)
  
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

;; DEVELOPMENT

#|
(set! %gnucash-data% (import-gnucash-file %path-data-file%))
(define account (send %gnucash-data% account-by-name "BMO (CELI"))
(send account get-children)
(define root (send %gnucash-data% account-by-name "Root Account"))



|#


;(children-rec root "")
;(displayln (children-tree root ""))

#|
(define accounts (send %gnucash-data% accounts-sorted-by-name))
;(define filtered-accounts (filter (lambda (act)
;          (member (send act get-type) (list "BOND" "MUTUAL" "STOCK"))) accounts));
(define filtered-accounts (filter (lambda (act) (send act is-investment?)) accounts))
(for ([act filtered-accounts])
  (printf "~a (~a)~%" (send act get-name) (send act get-type)))
|#

#|
;(for ([account (send %gnucash-data% accounts-sorted-by-name)])
;  (displayln (send account get-name)))
(define account-id (send (send %gnucash-data% account-by-name "BMO MasterCard") get-id))
(define request (void))
(define page-number 0)
(define %trans-per-page% 10)
;(define lines (bank-ledger-view %gnucash-data% account-id request page-number))

(define lines (trans-in-account->ledger-lines %gnucash-data% account-id page-number %trans-per-page%))
(flatten-trans-splits lines)
;define lines (bank-ledger-view %gnucash-data% account-id request page-number))
(printf "Length lines: ~a~%" (length lines))

|#