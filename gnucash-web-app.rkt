#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/templates         
         (file "lib/allocation.rkt")
         (file "lib/finance.rkt")
         (file "lib/gnucash-parser.rkt")
         (file "lib/gnucash-objects.rkt")
         (file "views/web-views.rkt"))

(define %global-gnucash-data% null) ; global set in (start-app)
(define %global-allocation-data% null) ; global set in (start-app)

 ; global to display error page with message when load fail (with-handlers)
(define %global-load-fail-error-text% "")
;(define %base-template-path% "templates/base-template.html")

;; ----------------
;;  CONFIG STRINGS
;; ----------------
;;(define DEFAULT-PATH-DATA-FILE "d:\\Documents\\gnucash\\michel.gnucash")
;(define DEFAULT-PATH-DATA-FILE "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\tests\\test-file1.gnucash")
(define DEFAULT-PATH-DATA-FILE "/media/michel/WindowsD/Documents/programming/racket/racket-projects/GnuCash/tests/test-file1.gnucash")
;;(define DEFAULT-PATH-DATA-FILE "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define DEFAULT-ALLOCATION-DATA-FILE "allocation-data.txt")

(define CATCH-EXCEPTIONS-ON #f)


;;
;; GLOBAL gnucash-file-name - set to default here, change in (startapp), can be displayed in any view
;;
(define %gnucash-file-full-path% DEFAULT-PATH-DATA-FILE)

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

;; ------------------
;;  RELOAD FUNCTIONS
;; ------------------
(define (reload-gnucash-data)
  (with-handlers
      ([exn:fail? (λ (e)
                    (set! %global-load-fail-error-text%
                          (format "ERROR STARTING WEB APP: ~a" (exn-message e))))])
    
    (printf "Reloading data file '~a'~%" %gnucash-file-full-path%) 
    (set! %global-gnucash-data% (import-gnucash-file %gnucash-file-full-path%))  
    (printf "Loading data complete.~%")
    (displayln "")  
    (print-overview)))

(define (reload-roi-report-view date url)
  (reload-gnucash-data)
  ; remove /reload? from the URL
  (define fixed-url (string-replace url "/reload" ""))
  (roi-report-view %global-gnucash-data% %gnucash-file-full-path% date fixed-url %global-allocation-data%))      


(define (reload-allocation-view date url)
  (reload-gnucash-data)
  (define fixed-url (string-replace url "/reload" ""))
  (allocation-view %global-gnucash-data% %gnucash-file-full-path% date fixed-url %global-allocation-data%))

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
   [("allocation" (string-arg))  #:method "get"
    (lambda (request date) (allocation-view %global-gnucash-data% %gnucash-file-full-path% date (get-url request) %global-allocation-data%))]      
   [("allocation" (string-arg)) #:method "post"
    (lambda (request date)
      (allocation-view %global-gnucash-data% %gnucash-file-full-path% date (get-url request) %global-allocation-data% (post-data/raw->hash request)))]
   [("allocation" (string-arg) "reload")  #:method "get"
    (lambda (request date)
      (reload-allocation-view date (get-url request)))]
   [("roi-report" (string-arg))
    (lambda (request date) (roi-report-view %global-gnucash-data% %gnucash-file-full-path% date (get-url request) %global-allocation-data%))]      
   [("roi-report" (string-arg) "reload")
    (lambda (request date) (reload-roi-report-view date (get-url request)))]      
   ;; id page-number split-flag (s1 display splits, else just trans)
   [("account" (string-arg) (integer-arg) (string-arg))
    (lambda (request account-id page-num split-flag)
      (ledger-view %global-gnucash-data% %gnucash-file-full-path% account-id request page-num split-flag (get-url request)))]
   [("accounts") (lambda (request) (account-list-view %global-gnucash-data% %gnucash-file-full-path% request))]

   [("") (lambda (request) (account-list-view %global-gnucash-data% %gnucash-file-full-path% request))]
   
   [("app-cannot-start") (lambda (request) (app-cannot-start-view %global-load-fail-error-text%))]
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

  (define command-line-args (current-command-line-arguments))
  (printf "Command line arguments: ~a~%" command-line-args)
  (when (= 1 (vector-length command-line-args))
        (set! %gnucash-file-full-path% (vector-ref command-line-args 0)))
 

  (define root-url "/") ; with handlers may change to error page
  (with-handlers
      ([exn:fail? (λ (e)
                    (set! %global-load-fail-error-text%
                          (format "ERROR STARTING WEB APP: ~a" (exn-message e))))])

    (displayln "Loading configuration.")
    (printf "Loading allocation file '~a'~%" DEFAULT-ALLOCATION-DATA-FILE) 
    (set! %global-allocation-data% (file->alloc-hash DEFAULT-ALLOCATION-DATA-FILE))
    (displayln "Configuration okay.")    
    (displayln "")  
    
    (printf "Loading data file '~a'~%" %gnucash-file-full-path%) 
    (set! %global-gnucash-data% (import-gnucash-file %gnucash-file-full-path%))  
    (printf "Loading data complete.~%")
    (displayln "")  
    (print-overview %global-gnucash-data%)
    (displayln "")
    (printf "Launching the web server.~%")

    )

  ;; Start the server.
  (serve/servlet request-handler
                 #:launch-browser? #t
                 #:servlet-path (if (equal? "" %global-load-fail-error-text%)
                                    "/"
                                    "/app-cannot-start")
                 #:quit? #f
                 #:listen-ip "127.0.0.1"
                 #:port 8000
                 #:servlet-regexp #rx"")  
)

(start-app)