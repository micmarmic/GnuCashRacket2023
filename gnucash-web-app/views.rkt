#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         racket/block
         racket/runtime-path ; build path to css files
         rackunit)

(require "pagination.rkt"
         "finance.rkt"
         "simple-date.rkt")

(provide (all-defined-out))


(define BOOTSTRAP-COLOR-TRANSACTION "table-success") ; 
(define BOOTSTRAP-COLOR-SPLIT "table-light") ;

(define HTML-TEXT-INDENT "  ")

;(define-runtime-path BASE-CSS-FILEPATH "static/css/base.css") ; to load as file

;; ------------
;;  ROI REPORT
;; ------------

(define (roi-report-view gnucash-data arg-date url)
  (printf "ROI REPORT \n~a~%" arg-date)
  (let* ([master-list-roi (roi-on-date gnucash-data arg-date)]
         [grand-total-line (calc-grand-total-list-account-roi master-list-roi)]
         [view-heading (format "ROI Report - ~a" arg-date)]
         [page-title (format "~a | GnuCash" view-heading)]
         [main-content-heading view-heading]
         [date-selector (make-date-selector gnucash-data arg-date url)]
         [form-url (substring url 0 (- (string-length url) 10))]
         [extra-javascript (include-template "static/js/date-selector.js")])
      (response-200-base-template page-title main-content-heading
                                    (include-template "templates/roi-view.html"))))


; given a date, gnucash-data, return values needed in
; date-selector.html
; return: form-url, list of values for select year, month, day
; with "selected" for the current one
(struct select-option (value selected))
; caveatlist of day is not dynamic - need javascript; always list 1 to 31; 
(define (make-date-selector gnucash-data arg-date url)
  (let* ([year (string->number (substring arg-date 0 4))]
        [month (string->number (substring arg-date 5 7))]
        [day (string->number (substring arg-date 8 10))]
        ; todo gnucash max-year min-year        
        [year-options (make-list-select-option 2018 2023 year #f)]
        [month-options (make-list-select-option 1 12 month #t)]
        [day-options (make-list-select-option 1 31 day #t)])
    (include-template "templates/date-selector.html")))


; to is inclusive, from 1 to 3 -> 1 2 3
(define (make-list-select-option from to selected ascending?)
  (let ([options (for/list ([n (in-range from (add1 to))]) ; add1 to include to
                   (select-option n (if (= n selected) "selected" "")))])
    (if ascending? options (reverse options))))
  
(define (print-list-select-option lst)
  (for ([opt lst])
    (printf "~a ~a~%" (select-option-value opt) (select-option-selected opt))))

; demo (print-list-select-option (make-list-select-option 1 12 4 #t))
;; --------------
;; LEDGER HELPERS
;; --------------

(define (flatten-trans-splits list-trans-splits)
  (let ([final-list '()])
    (for ([item (in-list list-trans-splits)])
      (set! final-list (append final-list (list (car item))))
      (for ([split (in-list (rest item))])
        (set! final-list (append final-list split))))
    (printf "length final-list ~a~%" (length final-list))
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
  (format "~a ~a ~a ~a ~a ~a ~a ~a"
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
;; FOR FULL LIST OF LEDGERS LINES, SET PAGE NUMBER TO < 0
;; returns (list of (list linefortrans (list linesfor splits

(define (bank-account->ledger-lines gnucash-data arg-account-id page-number split-flag items-per-page)
  (let* ([page-index (sub1 page-number)] ;work with 0-based page index, not 1-based page-number
         [account (send gnucash-data account-by-id arg-account-id)]
         [transactions (send account transactions-sorted-by-date)]
         [list-lines '()]
         [balance 0]
         [show-splits (equal? split-flag "s1")])
   (for ([trans (in-list transactions)])
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
                  "" 
                  "" 
                  (real->decimal-string total)
                  (real->decimal-string new-balance)
                  BOOTSTRAP-COLOR-TRANSACTION)
                 (bank-ledger-line
                  (send trans get-date)
                  (send trans get-description)
                  (send trans get-memo)
                  (split-account-or-plus trans arg-account-id)
                  ""
                  ""
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
    (for ([split (in-list arg-splits)])
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
   (for ([trans (in-list transactions)])
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
    (for ([split (in-list arg-splits)])
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
        ;[list-html (children-tree root-account start-indent)]
        ;[list-html (format "<ul>~a</ul>" (account-tree root-account start-indent))]
        [list-html (account-tree-no-root root-account start-indent)]
        [page-title "Accounts | GnuCash App"]
        [main-content-heading "Accounts"]
        [main-content (include-template "templates/account-list.html")])
    (response-200-base-template page-title main-content-heading main-content)))

(define (account-tree-no-root root-account start-indent)
  (let* ([account-tree (account-tree root-account start-indent)]
        [num-lines (length account-tree)]
        [trimmed-tree (if (<= num-lines 3) ; min tree is one account, 3 lines ul, li, /ul
                          account-tree
                          ; remove the first and last line,    
                          (take-n-from-x account-tree (- num-lines 3) 2))])
    (string-append (list-string->string trimmed-tree) "</ul>"))) ;; need a ul at the end?!
  

(define (account-tree account arg-indent)
  (let* ([children (send account get-children)]
         [account-link-or-not
          (if (> (send account num-transactions) 0)
                    (account-link account)
                    (send account get-name))]
         ; add span to expand, collapse if there are children
         [account-link-or-not (if (empty? children)
                                  account-link-or-not
                                  (string-append "<span>" account-link-or-not "</span>"))]
         [children-tree (if (empty? children)
                            '()
                            (flatten (list (format "~a<ul class=\"account-list\">" arg-indent)
                                  (for/list ([child (in-list children)])
                                              (account-tree child (string-append arg-indent HTML-TEXT-INDENT)))
                                           (format "~a~a" arg-indent "</ul>"))))])
         (if (empty? children)
             (list (format "~a<li>" arg-indent) (format "~a~a~a" arg-indent arg-indent account-link-or-not) (format "~a</li>" arg-indent))
             (flatten (list (format "~a<li>" arg-indent) (format "~a~a~a" arg-indent arg-indent account-link-or-not) children-tree)))))

           

(define (list-string->string lst)
  (foldr (lambda (s result) (string-append s "\n" result)) "" lst))
#|
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
                            (format "<li>~%~a<ul class=\"list-unstyled\">~%~a</ul></li>~%"
                                    child-link-name
                                    (children-tree child indent)))])
          (loop (rest children) (string-append results child-ul) new-indent)))))

        

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

;; ------------------------------
;;  BALANCE IN LEDGER AS OF DATE
;; ------------------------------

(define (balance-of-ledger-on-closest-date gnucash-data ledger-lines arg-date)
  (let loop ([lines ledger-lines] [found-balance null])
    (if (empty? lines)
        (if (null? found-balance)
            (error (format "didn't find the balance on ~a~%" arg-date))
            found-balance)
        ; else (list is not empty)
        (let* ([line (car lines)]
               [date (bank-ledger-line-date line)])
          (cond
            [(equal? date arg-date) (bank-ledger-line-balance line)]
            [(string<? date arg-date) (loop (rest lines) (bank-ledger-line-balance line))] ; closest price so far
            [(string>? date arg-date)
             (if (null? found-balance)
                 (error (format "didn't find the balance on ~a~%" date))
                 found-balance)])))
  found-balance))

;; -------------
;; VARIOUS DEMOS
;; -------------

(define (dashboard request)
  (let* ([page-title "Dashboard | GnuCash App"]
        [main-content-heading "Dashboard"]
        [clients (list (list "Smith" "Mark") (list "Simpson" "Lou"))]
        [main-content (include-template "templates/My-Dashboard.html")])
    (response-200-base-template page-title main-content-heading main-content)))


(define (generic-404 request)
  (let ([request-url (url->string (request-uri request))])
     (http-response-404 (format "Cannot dispatch URL: '~a'" request-url))))

;; ----------------
;;  HTML RESPONSES
;; ----------------

(define (response-200-base-template page-title main-content-heading main-content)
  (let* ([base-css (include-template "static/css/base.css")]
         [bootstrap-js-links (include-template "templates/bootstrap-js-links.txt")]                  
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

