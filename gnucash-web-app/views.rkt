#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         racket/block
         racket/runtime-path ; build path to css files
         rackunit)

(require "pagination.rkt"
         "finance.rkt")

(provide (all-defined-out))


(define BOOTSTRAP-COLOR-TRANSACTION "table-success") ; 
(define BOOTSTRAP-COLOR-SPLIT "table-light") ;

;(define-runtime-path BASE-CSS-FILEPATH "static/css/base.css") ; to load as file

;; ------------
;;  ROI REPORT
;; ------------

(define (roi-report-view gnucash-data arg-date)
  (let* ([master-list-roi (roi-on-date gnucash-data arg-date)]
         [grand-total-line (calc-grand-total-list-account-roi master-list-roi)]
         [view-heading (format "ROI Report - ~a" arg-date)]
         [page-title (format "~a | GnuCash" view-heading)]
         [main-content-heading view-heading])
    (response-200-base-template page-title main-content-heading
                                    (include-template "templates/roi-view.html"))))


;; --------------
;; LEDGER HELPERS
;; --------------

(define (flatten-trans-splits list-trans-splits)
  (let ([final-list '()])
    (for ([item list-trans-splits])
      (set! final-list (append final-list (list (car item))))
      (for ([split (rest item)])
        (set! final-list (append final-list split))))
    final-list))

;; -----------
;; LEDGER VIEW
;; -----------

; expect page-uril /account/account-id/page-num/s0 or s1
(define (make-split-toggle-link page-uri)
  ; some duplication in make-navigation, just once
  (let* ([split-uri (string-split page-uri "/")]
        [split-flag (if (equal? "s0" (last split-uri)) "s1" "s0")])
         ;; new-uri needs leading /
    ;                       "account"              account-id            page0-num          split
    (string-append "/" (first split-uri)  ; "/account"
                   "/" (second split-uri) ; "/account-id"
                   "/" (third split-uri)  ; "/page-num
                   "/" split-flag)))      ; "/s0 or s1

;; process request for ledger view and trigger response
;; based on account-type, refer to bank or investment ledger
(define (ledger-view gnucash-data arg-account-id request page-number split-flag page-url)
  (let* ([account (send gnucash-data account-by-id arg-account-id)]
         [link-toggle-splits (make-split-toggle-link page-url)]
         [account-name (send account get-name)]
         [view-heading  (format "~a" account-name)]
         [page-title (format "~a | GnuCash App" view-heading)]
         [main-content-heading view-heading]
         ; nav works with page-number (1-based)
         [num-trans (send account num-transactions)]
         [num-pages (ceiling (/ num-trans %trans-per-page%))]
         [navigation-bundles (make-navigation page-url num-trans %trans-per-page% page-number)]
         [prev-link (if (empty? navigation-bundles) "" (nav-bundle-prev-link navigation-bundles))]
         [next-link (if (empty? navigation-bundles) "" (nav-bundle-next-link navigation-bundles))]
         ;; other links is truncated all-nav minus first and last (prev and next)
         [other-links (if (< num-trans %trans-per-page%)
                          (nav-bundle-page-links navigation-bundles)
                          (truncated-pagination (nav-bundle-page-links navigation-bundles) page-number))]
         ;;[other-links (if (null? navigation-bundles) (void) (nav-bundle-page-links navigation-bundles))]
         ;[navigation "<p>nav disabled</p>"])
         [navigation (if (= num-pages 1) "" (include-template "templates/navigation.html"))])
    (if (send account is-investment?)
        (let ([ledger-lines (flatten-trans-splits
           (investment-account->ledger-lines gnucash-data arg-account-id page-number %trans-per-page%))])
          (response-200-base-template page-title main-content-heading
                                    (include-template "templates/investment-ledger.html")))
        (let ([ledger-lines (flatten-trans-splits                             
           (bank-account->ledger-lines gnucash-data arg-account-id page-number split-flag %trans-per-page%))])
          (response-200-base-template page-title main-content-heading
                                    (include-template "templates/bank-ledger.html"))))
    ))





;; all numbers are 1-based
;; current-page: the page the user is on
;; page-number: the counter in the loop calling this function
;; last-page: the number of the last page
(define (make-page-link page-uri page-num current-page)
  (let* ([split-uri (string-split page-uri "/")]
         [uri (string-append
               "/" (first split-uri)  ; "/account"
               "/" (second split-uri) ; "/account-id"
               "/" (~a page-num)  ; "/page-num 
               "/" (last split-uri))]      ; "/s0 or s1
         [li-class (if (= current-page page-num) "page-item active" "page-item")])
    (nav-link uri (~a page-num) li-class ""))) ; no tab-index

;; return a nav-package with values for previous, pages, next nav links
;; NOTE duplication with build uri
(define (make-navigation page-uri num-items items-per-page current-page)
  (let* ([split-uri (string-split page-uri "/")]
         [num-pages (ceiling (/ num-items items-per-page))]
         ;; new-uri needs leading /
         ;;                                   account                         
         [previous (if (= current-page 1)
                       ;; TODO: when disabled tab-index=-1
                    (nav-link "#" "Previous" "page-item disabled" " tabindex=\"-1\"")
                    (nav-link
                     (string-append "/" (first split-uri)  ; "/account"
                                    "/" (second split-uri) ; "/account-id"
                                    "/" (~a (sub1 current-page))  ; "/page-num 
                                    "/" (last split-uri))      ; "/s0 or s1
                     "Previous"
                     "page-item"
                     ""))] ; no tab-index
         [next (if (= current-page num-pages)
                    (nav-link "#" "Next" "page-item disabled" " tabindex=\"-1\"")
                    (nav-link (string-append
                               "/" (first split-uri)  ; "/account"
                               "/" (second split-uri) ; "/account-id"price-list-for-cmdty-id
                               "/" (~a (add1 current-page))  ; "/page-num 
                               "/" (last split-uri))      ; "/s0 or s1
                     "Previous"
                     "page-item"
                     ""))]) ; no tab-index
    (let ([all-links
           (let loop ([page-num 1] [links '()])
             (cond [(> page-num num-pages) links]
                   [else
                    (loop (add1 page-num)
                          (append links
                                  (list (make-page-link page-uri page-num current-page))))]))])
      (nav-bundle previous all-links next))))
                             

;; DEMO
#|
(displayln "navigation for \"account/2\" 51 10 6")
(define bundle (make-navigation "account/2" 51 10 6))
(display-nav-link (nav-bundle-prev-link bundle))
(let ([links (nav-bundle-page-links bundle)])
  (for ([link links])
    (display-nav-link link)))
(display-nav-link (nav-bundle-next-link bundle))
|#

;; ------------
;; BANK LEDGERS
;; ------------

;; ledger-line to display in HTML table
;; color-class for css class to draw transaction line different from split
(struct bank-ledger-line (date description memo account-name quantity value total balance color-class))
;; return a list of ledger lines and transaction value in one struct
(struct bank-compiled-splits (ledger-lines transaction-total))

(define (list-ledger-lines->string arg-lines)
  (let loop ([lines arg-lines] [result ""])
    (if (null? lines)
        result
        (loop (rest lines)
              (string-append result (ledger-line->string (car lines)) "\n")))))

(define (ledger-line->string line)
  ;(format "dt:~a desc:~a act:~a val:~a tot:~a bal:~a"
  (format "~a ~a ~a ~a ~a ~a"
          (~a (bank-ledger-line-date line) #:width 12)
          (~a (bank-ledger-line-description line) #:width 30)
          (~a (bank-ledger-line-account-name line) #:width 30)
          (~a (bank-ledger-line-quantity line) #:width 10)
          (~a (bank-ledger-line-value line) #:width 10)
          (~a (bank-ledger-line-total line) #:width 10)
          (~a (bank-ledger-line-balance line) #:width 10)
          (~a (bank-ledger-line-color-class line) #:width 20)))


; if trans has only two splits (current account and other account)
; return the account name, else "(plus)"
(define (split-account-or-plus trans arg-account-id)
  (let ([splits (send trans get-splits)])    
    (cond [(= 2 (length splits))
           (let ([other-split (car (filter (lambda (s) (not (equal? arg-account-id (send s get-account-id)))) splits))])
             (send other-split get-account-name))]
          [else "(plus)"])))


;; compile a list of ledger-lines for trans and splits in given account-id
;; page-number and items-per-page for pagination - see (sublist lst sublist-from sublist-len)
(define (bank-account->ledger-lines gnucash-data arg-account-id page-number split-flag items-per-page)
  (let* ([page-index (sub1 page-number)] ;work with 0-based page index, not 1-based page-number
         [account (send gnucash-data account-by-id arg-account-id)]
         [transactions (send account transactions-sorted-by-date)]
         [list-lines '()]
         [balance 0]
         [show-splits (equal? split-flag "s1")])
   (for ([trans transactions])
     (let* ([compiled (compile-bank-splits (send trans get-splits) arg-account-id)]
           [total (bank-compiled-splits-transaction-total compiled)]
           [new-balance (+ balance total)]           
           [trans-line
            (if show-splits
                (bank-ledger-line
                 (send trans get-date)
                 (send trans get-description)
                 (send trans get-memo)
                 ""
                 "" ; quantity
                 "" ; value
                 (real->decimal-string total)
                 (real->decimal-string new-balance)
                 BOOTSTRAP-COLOR-TRANSACTION)
                (bank-ledger-line
                 (send trans get-date)
                 (send trans get-description)
                 (send trans get-memo)
                 (split-account-or-plus trans arg-account-id)
                 "" ; quantity
                 "" ; value
                 (real->decimal-string total)
                 (real->decimal-string new-balance)
                 BOOTSTRAP-COLOR-TRANSACTION))]
           ;[split-lines (bank-compiled-splits-ledger-lines compiled)]
           [split-lines (if show-splits
                            (bank-compiled-splits-ledger-lines compiled)
                            '())]
           )
       ;; list list so trans info is not merged into big list
       (set! list-lines (append list-lines (list (list trans-line split-lines))))
                                ;(compiled-splits-ledger-lines compiled)))
       (set! balance new-balance)))
    ;; paginate here because we need the balance to be calc from the start of the list
    ;; work with 0-based page index
    (if (>= page-index 0)
        (take-n-from-x list-lines items-per-page (* (sub1 page-number)  items-per-page))
        list-lines)))


;; return a list of ledger-line struct and the amount of the transaction
;; use struct compiled-splits to return two values
(define (compile-bank-splits arg-splits arg-account-id)
  (let ([result-lines '()]
        [amount 0])
    (for ([split arg-splits])
      (if (equal? (send split get-account-id) arg-account-id)
          ; split with current account: don't add a split
          ; set the transaction amount
          (set! amount (send split get-value))
          ; else: split for other account: create ledger line
          (set! result-lines
                (append result-lines
                        (list ; 'list' in order to append to existing list
                         (bank-ledger-line "" ; date
                                           "" ; description
                                           (send split get-memo)
                                           (send split get-account-name)
                                           (real->decimal-string (send split get-quantity))
                                           (real->decimal-string (send split get-value))
                                           "" ; total
                                           "" ; balance
                                           BOOTSTRAP-COLOR-SPLIT)))))) 
    (bank-compiled-splits result-lines amount)))


;; -----------------
;; INVESTMENT LEDGER
;; -----------------



(struct investment-ledger-line (date description account-name quantity price value balance color-class))
(struct investment-compiled-splits (ledger-lines num-shares))

;; compile a list of ledger-lines for trans and splits in given account-id
;; page-number and items-per-page for pagination - see (sublist lst sublist-from sublist-len)
(define (investment-account->ledger-lines gnucash-data arg-account-id page-number items-per-page)
  (let* ([account (send gnucash-data account-by-id arg-account-id)]
         [transactions (send account transactions-sorted-by-date)]
         [list-lines '()]
         [balance 0])
   (for ([trans transactions])
     (let* ([compiled (compile-investment-splits (send trans get-splits) arg-account-id)]
           [num-shares (investment-compiled-splits-num-shares compiled)]
           [new-balance (+ balance num-shares)]
           [trans-line (investment-ledger-line
                        (send trans get-date)
                        (send trans get-description)                        
                        "" ; account name
                        (real->decimal-string num-shares)
                        "" ; price
                        "" ; amount
                        (real->decimal-string new-balance)
                        BOOTSTRAP-COLOR-TRANSACTION)]
           [split-lines (investment-compiled-splits-ledger-lines compiled)])
       ;; list list so trans info is not merged into big list
       (set! list-lines (append list-lines (list (list trans-line split-lines))))
                                ;(compiled-splits-ledger-lines compiled)))      
       (set! balance new-balance)))
    ;; paginate here because we need the balance to be calc from the start of the list
    (if (>= page-number 0)
        (take-n-from-x list-lines items-per-page (* (sub1 page-number) items-per-page))
        list-lines)))


;; return a list of ledger-line struct and the number of shares of the transaction
;; careful: there can be more than one split for the commodity, for ex. buy 100 @ 2.22 and 200 @ 2.23
;; use struct investment-compiled-splits to return two values
(define (compile-investment-splits arg-splits arg-account-id)
  (let ([result-lines '()]
        [num-shares 0])
    (for ([split arg-splits])
      (if (equal? (send split get-account-id) arg-account-id)
          ; split for current investement account: shares price amount
          (let* ([qty (send split get-quantity)]
                [price (if (zero? qty) 0 (/ (send split get-value) (send split get-quantity)))])
           (set! result-lines
                 (append result-lines
                         (list ; 'list' in order to append to existing list                          
                         ;(struct investment-ledger-line (date description account-name quantity price value balance color-class))                         
                          (investment-ledger-line "" ; date
                                       (send split get-memo)
                                       "" ; account-name
                                       (real->decimal-string qty)
                                       (real->decimal-string price)                                       
                                       (real->decimal-string (send split get-value))
                                       "" ; balance
                                       BOOTSTRAP-COLOR-SPLIT))))
           ; save num shares for transaction display
           ; increase num-shares here - there may be more than one split for the commodity in the same t
           (set! num-shares (+ num-shares (send split get-quantity))))
          ; else: split for other account
          (set! result-lines
                (append result-lines
                        (list ; 'list' in order to append to existing list
                         (investment-ledger-line "" ; date
                                      (send split get-memo)
                                      (send split get-account-name)
                                      "" ; quantity
                                      "" ; price
                                      (real->decimal-string (send split get-value))
                                      "" ; balance
                                      BOOTSTRAP-COLOR-SPLIT))))))
    (investment-compiled-splits result-lines num-shares)))


;; -------------------------
;; ACCOUNTS AND ACCOUNT LIST
;; -------------------------

(define (account-details gnucash-data request id)
  (let* ([account (send gnucash-data account-by-id id)]
        [account-str (send account as-string)]
        [back-url "<a href=\"/accounts\">List accounts</a>"])
    ;(printf "Trying to get account id '~a'. Got: ~a" id account)
    (http-response-200 (format "<p>Account as string: ~a</p>\n<p>~a</p>" account-str back-url))))


; list of name-link of account with transactions, else just name
(define (account-link account)
  (let* ([num-trans (send account num-transactions)]
         [account-name (format "~a (~a)" (send account get-name) num-trans)]         
         [href (string-append "/account/" (send account get-id) "/1/s0")]) ; default page 1, default: don't show splits
    (if (> (send account num-transactions) 0)
              (format "<a href=\"~a\">~a</a>" href account-name) 
              account-name)))

(define (account-list gnucash-data request)
  (let* ([root-account (send gnucash-data get-root-account)]
        [start-indent ""]
        [list-html (children-tree root-account start-indent)]
        [page-title "Accounts | GnuCash App"]
        [main-content-heading "Accounts"]
        [main-content (include-template "templates/account-list.html")])
      (response-200-base-template page-title main-content-heading main-content)))

(define (children-tree account arg-indent)
  (let loop ([children (send account get-children)]
             [results ""]
             [indent arg-indent])
    (if (empty? children)
        results
        (let* ([new-indent (string-append indent indent)]
               [child (first children)]
               [child-name (send child get-name)]
               [child-children (send child get-children)]
               [child-link-name
                (if (> (send child num-transactions) 0)
                    (account-link child)
                    child-name)]
               [child-ul (if (empty? child-children)
                            (format "<li class=\"mb-2\">~a</li>~%" child-link-name)
                            (format "<ul class=\"list-unstyled\"><li>~%~a<ul class=\"list-unstyle\">~%~a</ul></li></ul>~%"
                                    child-link-name
                                    (children-tree child indent)))])
          (loop (rest children) (string-append results child-ul) new-indent)))))
        
#|    

; display the account list view
; pass a list of <a href...> to the template because empty account will
; not have a link, and other wills
(define (account-list gnucash-data request)
  ; inner function: build list of href for each account, or just name if not transactions  

  ; actual code
    (let* ([page-title "Accounts | GnuCash App"]
           [main-content-heading "Accounts"]
           [links (map account-link (send gnucash-data accounts-sorted-by-name))]
           [main-content (include-template "templates/account-list.html")])
      (response-200-base-template page-title main-content-heading main-content)))
|#

;; build an html nested list of accounts with links of they have transaction
;; use Bootstrap styling
;;    <ul class="list-unstyled">
;;      <li class="mb-2"><a href="uri-for-account">account name</a></li>
;;    </ul>
;; don't show root 
;;   Expenses
;;       Rent
;;       Food
;;   Income
;;       Salary       
(define (account-tree-view gnucash-data)
  #t)



;; -------------
;; VARIOUS DEMOS
;; -------------

#|
(define (parents-demo request)
  (struct parent (name children))
  (struct child (name))
  (define parents
    (list
     (parent "Josie"(list (child "Bill") (child "Linus")))
     (parent "Jack"(list (child "Sam") (child "Shelly")))))
  (define (ul-children parent)
    (let ([children (parent-children parent)])    
      (if (null? children)
          "<p>NO CHILDREN</p>"
          (include-template "templates/demo-children.html"))))                                                         
  (define (ul-parents parents)
    (include-template "templates/demo-parents.html"))
  (let* ([page-title "Parents | GnuCash App"]
        [main-content-heading "Parents"]
        [main-content (ul-parents parents)])
    (response-200-base-template page-title main-content-heading main-content)))
|#
(define (parents-demo request)
  (struct parent (name children))
  (define parents
    (list
     (parent "Max"
             (list
              (parent "Josie"
                      (list (parent "Bill" '()) (parent "Linus"  '())))))
     (parent "Jack"
             (list
              (parent "Sam"  '())
              (parent "Shelly" '())))
    ))
  (let* ([page-title "Parents | GnuCash App"]
        [main-content-heading "Parents"]
        [main-content (include-template "templates/demo-children.html")])
    (response-200-base-template page-title main-content-heading main-content)))
  

(define (hello-root request)
  (let* ([page-title "Home | GnuCash App"]
        [main-content-heading "Home"]
        [main-content ""])
    (response-200-base-template page-title main-content-heading main-content)))


(define (dashboard request)
  (let* ([page-title "Dashboard | GnuCash App"]
        [main-content-heading "Dashboard"]
        [clients (list (list "Smith" "Mark") (list "Simpson" "Lou"))]
        [main-content (include-template "templates/My-Dashboard.html")])
    (response-200-base-template page-title main-content-heading main-content)))


(define (generic-404 request)
  (let ([request-url (url->string (request-uri request))])
     (http-response-404 (format "Cannot dispatch URL: '~a'" request-url))))



;; ----------
;;  BASE CSS
;; ----------
(define (load-base-css)
  ; if you load a file, it has to remain on disk
  ;(file->string BASE-CSS-FILEPATH))
  ; import-template is compiled with the program
  (include-template "static/css/base.css"))

;; ----------------
;;  HTML RESPONSES
;; ----------------

(define (response-200-base-template page-title main-content-heading main-content)
  (let* ([base-css (load-base-css)]
        [html (include-template "templates/base-template.html")])
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

;; ----------
;; PAGINATION
;; ----------

#|
<nav aria-label="...">
  <ul class="pagination">
    <li class="page-item disabled">
      <a class="page-link" href="#" tabindex="-1">Previous</a>
    </li>
    <li class="page-item"><a class="page-link" href="#">1</a></li>
    <li class="page-item active">
      <a class="page-link" href="#">2 <span class="sr-only">(current)</span></a>
    </li>
    <li class="page-item"><a class="page-link" href="#">3</a></li>
    <li class="page-item">
      <a class="page-link" href="#">Next</a>
    </li>
  </ul>
</nav>
|#
