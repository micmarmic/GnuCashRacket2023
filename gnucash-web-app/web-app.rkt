#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/templates         
         "gnucash-parser.rkt"
         "gnucash-objects.rkt"
         "views.rkt"
         "finance.rkt"
         "allocation.rkt")

(define %global-gnucash-data% null) ; global set in (start-app)
(define %global-allocation-data% null) ; global set in (start-app)
;(define %base-template-path% "templates/base-template.html")

;; ----------------
;;  CONFIG STRINGS
;; ----------------
;(define DEFAULT-PATH-DATA-FILE "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define DEFAULT-PATH-DATA-FILE "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\gnucash-web-app\\tests\\test-file1.gnucash")
(define DEFAULT-ALLOCATION-DATA-FILE "allocation-data.txt")

(define CATCH-EXCEPTIONS-ON #f)



;(define DEFAULT-PATH-DATA-FILE "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")

;; TODO view status check: transfer to same account, unbalanced and orphaned transactions

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
  (let* ([account (send %global-gnucash-data% account-by-id id)]
        [account-str (send account as-string)]
        [back-url "<a href=\"/accounts\">List accounts</a>"])
    (printf "Trying to get account id '~a'. Got: ~a" id account)
    (http-response-200 (format "<p>Account as string: ~a</p>\n<p>~a</p>" account-str back-url))))

(define (account-list request)
  (define (accounts-names-urls)
    (define (name-url account)
      (list (send account get-name) (string-append "/account/" (send account get-id))))
    (map name-url (send %global-gnucash-data% accounts-sorted-by-name)))
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

#|
(define (dashboard request)
  (let ([clients (list (list "Smith" "Mark") (list "Simpson" "Lou"))])
    (include-template "templates/My-Dashboard.html")
    (http-response-200 (include-template "templates/My-Dashboard.html"))))
|#

;;
;; GENERIC EXCEPTION HANDLER
;;

;; Call this from the dispatch to wrap all calls in exception handler

;; -----------------------------------
;; URL routing table (URL dispatcher).
;; -----------------------------------

(define (get-url request)
  ;(printf "====~a~%" (url->string (request-uri request)))
  (url->string (request-uri request)))

; field1=&field2=dasfd -> hash (field1 "") (field2 "dasfd")
(define (post-data/raw->hash request)  
  (define data (~a (request-post-data/raw request)))
  (define fields (if (string-contains? data "&")
                     (string-split data "&")
                     (list data)))
  (define field-hash
    (make-hash
     (for/list ([raw-field (in-list fields)])
       (define field-data (string-split raw-field "="))
       (if (= 1 (length field-data))
           (list (first field-data) "")
           (list (first field-data) (second field-data))))))
  field-hash)

  
  
    
#|
(with-handlers ([exn:fail? (λ (e) (printf "EXCEPTION: ~a~%" (exn-message e)))])
  (raise (my-exception "FORCED ERROR" (current-continuation-marks))))
|#

(define-values (dispatch generate-url)
  (dispatch-rules
   [("allocation" (string-arg))
    (lambda (request date) (allocation-view %global-gnucash-data% date (get-url request) %global-allocation-data%))]      
   [("roi-report" (string-arg))
    (lambda (request date) (roi-report-view %global-gnucash-data% date (get-url request) %global-allocation-data%))]      
   ;; id page-number split-flag (s1 display splits, else just trans)
   [("account" (string-arg) (integer-arg) (string-arg))
    (lambda (request account-id page-num split-flag)
      (ledger-view %global-gnucash-data% account-id request page-num split-flag (get-url request)))]
   [("accounts") (lambda (request) (account-list-view %global-gnucash-data% request))]

   [("") (lambda (request) (account-list-view %global-gnucash-data% request))]
   
   [("dashboard") dashboard-view]
   [("test-form") #:method "get" (lambda (request) (test-form-get-view (get-url request)))]
   [("test-form") #:method "post" (lambda (request)
                                    (test-form-post-view (get-url request) (post-data/raw->hash request)))]
   [else generic-404]))

(define (request-handler request)
  (printf "Request path: ~a~%" (url->string (request-uri request)))
  (if CATCH-EXCEPTIONS-ON
      (with-handlers ([exn:fail? (λ (e) (displayln e)(exception-page (exn-message e)))])
        (dispatch request))
      (dispatch request)))

(define (start-app) 
  (displayln "Loading configuration.")    
  (set! %global-allocation-data% (file->alloc-hash DEFAULT-ALLOCATION-DATA-FILE))
  (displayln "Configuration okay.")    
  (displayln "")  

  (printf "Loading data file '~a'~%" DEFAULT-PATH-DATA-FILE) 
  (set! %global-gnucash-data% (import-gnucash-file DEFAULT-PATH-DATA-FILE))  
  (printf "Loading data complete.~%")
  (displayln "")  
  (print-overview %global-gnucash-data%)
  (displayln "")  

  (printf "Launching the web server.~%")
  ;; Start the server.
  (serve/servlet request-handler
                 #:launch-browser? #t
                 #:servlet-path "/"
                 #:quit? #f
                 #:listen-ip "127.0.0.1"
                 #:port 8000
                 #:servlet-regexp #rx""))

(start-app)