#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         racket/block
         racket/runtime-path ; build path to css files
         rackunit)

(require (file "pagination.rkt")
         (file "../lib/finance.rkt")
         (file "../lib/simple-date.rkt")
         (file "../settings.rkt")
         (file "../lib/allocation.rkt")
         (file "../lib/formatting-utils.rkt")
         )

(provide ledger-view
         
         generic-404
         app-cannot-start-view
         response-200-base-template
         exception-page
         dashboard-view
         account-list-view
         roi-report-view
         allocation-view

         test-form-post-view
         test-form-get-view

         adjust-fields adjust-fields-ca adjust-fields-us adjust-fields-intl
         adjust-fields-fixed adjust-fields-other
         
         strip-decimal
         )

;;
;; GENERAL HELPERS
;;

;; "12341.34" -> "12341"
;; "22" -> "22"
(define (strip-decimal str-value)
  (if (string-contains? str-value ".")
      (first (string-split str-value "."))
      str-value))


;;
;; DEBUG LET EXCEPTIONS RISE UP
;;
(define CATCH-EXCEPTIONS-ON #f)

(struct my-exception exn:fail:user ())

(define BOOTSTRAP-COLOR-TRANSACTION "table-success") ; 
(define BOOTSTRAP-COLOR-SPLIT "table-light") ;

(define HTML-TEXT-INDENT "  ")

;(define-runtime-path BASE-CSS-FILEPATH "static/css/base.css") ; to load as file

;;
;; EXCEPTION HANDLER
;; 
(define (exn-handler e)
  (if CATCH-EXCEPTIONS-ON
      (exception-page (exn-message e))
      (raise e)))

;;
;; Test GET vs PUT
;; 
(define (test-form-get-view url)
    (let* ([view-heading (format "TEST - GET")]
           [page-title (format "~a | GnuCash" view-heading)]
           [main-content-heading view-heading]
           [form-url url]
           [fname-value ""]
           [lname-value ""]
           [view-data  "NO DATA"]
           [form (include-template "templates/test-form.html")])           
      (response-200-base-template page-title main-content-heading
                                  (include-template "templates/test-form-view.html"))))

;;
;; Test GET vs PUT
;; 
(define (test-form-post-view url data-hash)
    (let* ([view-heading (format "TEST - POST")]
           [page-title (format "~a | GnuCash" view-heading)]
           [main-content-heading "<h1>test-form method POST</h1>"]
           [fname-value (hash-ref data-hash "fname")]
           [lname-value (hash-ref data-hash "lname")]
           [view-data (~a data-hash)]
           [form-url url]
           [form (include-template "templates/test-form.html")])
      (response-200-base-template page-title main-content-heading
                                  (include-template "templates/test-form-view.html"))))

;; ------------
;;  ROI REPORT
;; ------------

;  (with-handlers ([exn:fail? (λ (e) (printf "EXCEPTION: ~a~%" (exn-message e)))])


;; DEV NOTE: roi-on-date then calc-grand-total... yields a line with the total value
;; and the total allocation for all accounts, READY for allocation view

(define (roi-report-view gnucash-data gnucash-file-path arg-date url [alloc-hash null])
  ;(with-handlers ([exn:fail? (λ (e) (displayln e)(exception-page (exn-message e)))])
  ;(with-handlers ([exn:fail? (λ (e) (exn-handler e))])
    (let* ([master-list-roi (roi-on-date gnucash-data arg-date alloc-hash)]
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
        [year-options (make-list-select-option
                       (send gnucash-data get-min-trans-year)
                       (send gnucash-data get-max-trans-year)
                       year
                       #f)]
        [month-options (make-list-select-option 1 12 month #t)]
        [day-options (make-list-select-option 1 31 day #t)])
    (include-template "templates/date-selector.html")))


; to is inclusive, from 1 to 3 -> 1 2 3
(define (make-list-select-option from to selected ascending?)
  (let ([options
         (for/list ([n (in-range from (add1 to))]) ; add1 to include to
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
    final-list))


;; -----------
;; LEDGER VIEW
;; -----------
; expect page-uril /account/account-id/page-num/hide or show
(define (make-split-toggle-link page-uri)
  ; some duplication in make-navigation, just once
  (let* ([split-uri (string-split page-uri "/")]
        [split-flag (if (equal? "hide" (last split-uri)) "show" "hide")])
         ;; new-uri needs leading /
    ;                       "account"              account-id            page0-num          split
    (string-append "/" (first split-uri)  ; "/account"
                   "/" (second split-uri) ; "/account-id"
                   "/" (third split-uri)  ; "/page-num
                   "/" split-flag)))      ; "/hide or show

;; process request for ledger view and trigger response
;; based on account-type, refer to bank or investment ledger
(define (ledger-view gnucash-data gnucash-file-path arg-account-id request page-number split-flag page-url)
  (let* ([account (send gnucash-data account-by-id arg-account-id)]
         [link-toggle-splits (make-split-toggle-link page-url)]
         [account-name (send account get-fullname)]
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
               "/" (last split-uri))]      ; "/hide or show
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
                                    "/" (last split-uri))      ; "/hide or show
                     "Previous"
                     "page-item"
                     ""))] ; no tab-index
         [next (if (= current-page num-pages)
                    (nav-link "#" "Next" "page-item disabled" " tabindex=\"-1\"")
                    (nav-link (string-append
                               "/" (first split-uri)  ; "/account"
                               "/" (second split-uri) ; "/account-id"price-list-for-cmdty-id
                               "/" (~a (add1 current-page))  ; "/page-num 
                               "/" (last split-uri))      ; "/hide or show
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


;; if trans has only two splits (current account and other account)
;; return the account name of the "other" account, else "(plus)"
;; ex. in Chequing, split accounts Chequing and Rent, return Rent
(define (split-account-name-or-plus trans arg-account-id)
  (define splits (send trans get-splits))
  (cond
    [(not (= 2 (length splits))) "(plus)"]
    [else 
     (if (equal? (send (first splits) get-account-id) arg-account-id)
         (send (second splits) get-account-name)         
         (send (first splits) get-account-name))]))
    


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
         [show-splits (equal? split-flag "show")])
   (for ([trans (in-list transactions)])
     (let* ([compiled (compile-bank-splits
                       (send trans get-splits)
                       arg-account-id
                       (send trans get-date))]
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
                  (split-account-name-or-plus trans arg-account-id)
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
(define (compile-bank-splits arg-splits arg-account-id date-for-error)
  ;; ERROR CHECK: don't assume there's only one split to the current account
  ;; we found a transaction with two splits, both to the same account
  (define splits-with-current-account
    (filter (lambda (split)
              (equal? arg-account-id (send split get-account-id)))       
              arg-splits))  
  (when (> (length splits-with-current-account) 1)
    (error (format "FAILED. Transfer to same account ~a on ~a"
                   (send (first arg-splits) get-account-name)
                   date-for-error)))
  (define result-lines '())
  (define amount 0)
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
  (bank-compiled-splits result-lines amount))


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

(define (account-details gnucash-data gnucash-file-path request id)
  (let* ([account (send gnucash-data account-by-id id)]
        [account-str (send account as-string)]
        [back-url "<a href=\"/accounts\">List accounts</a>"])
    ;(printf "Trying to get account id '~a'. Got: ~a" id account)
    (http-response-200 (format "<p>Account as string: ~a</p>\n<p>~a</p>" account-str back-url))))


; list of name-link of account with transactions, else just name
(define (account-link account)
  (let* ([num-trans (send account num-transactions)]
         [account-name (format "~a (~a)" (send account get-name) num-trans)]         
         [href (string-append "/account/" (send account get-id) "/1/hide")]) ; default page 1, default: don't show splits
    (if (> (send account num-transactions) 0)
              (format "<a href=\"~a\">~a</a>" href account-name) 
              account-name)))

(define (account-list-view gnucash-data gnucash-file-path request)
  (let* ([root-account (send gnucash-data get-root-account)]
        [start-indent ""]
        ;[list-html (children-tree root-account start-indent)]
        ;[list-html (format "<ul>~a</ul>" (account-tree root-account start-indent))]
        [list-html (account-tree-no-root root-account start-indent)]
        [page-title "Accounts | GnuCash App"]
        [main-content-heading "Accounts"]
        ;; DEMO ERROR MESSAGE AT TOP OF SCREEN [message "Allo from account-list-view"]
        [message ""]
        [main-content (include-template "templates/account-list.html")]
        )
    (response-200-base-template page-title main-content-heading main-content message)))

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

;; -----------------------
;;  ASSET ALLOCATION VIEW
;; -----------------------


#|
;; get the allocation targets
;; get allocation for each commodity from file
;; get the roi-lines and calc for each commodity and totals
(define (allocation-view gnucash-data arg-date)
  (let* ([allocation-hash (get-allocation-hash)]
         [target-allocation (hash-ref allocation-hash "TARGET")]
         [view-heading (format "Asset Allocation")]
         [page-title (format "~a | GnuCash" view-heading)]
         [main-content-heading view-heading])
         (response-200-base-template page-title main-content-heading
                                     (include-template "templates/allocation-view.html"))))
|#


;; skip target, then sort commodities alphabetically
(define (alloc-rec-except-target allocation-hash)
  (let ([filtered (filter (lambda (rec) (not (equal? "TARGET" (alloc-rec-commodity rec))))
                          (hash-values allocation-hash))])
    (sort filtered alloc-rec<?)))


(struct adjust-fields (ca us intl fixed other))

;; given the entry in a data-hash from a form
;; return a number representation, treating empty as zero
(define (input-type-number->number value-in-hash)
  ;; value is a list; empty field is '("")
  (cond [(equal? '("") value-in-hash) 0]
        [else
         (string->number (first value-in-hash))]))
  
#|
(define (data-hash->adjust-fields data-hash current-total-value)
  (cond [(null? data-hash) (roi-line "Blank roi" 0 0 0 0 0 0 "" 0 0 0 0 0)]
        [else
         (roi-line "Adjusted Allocation Percent" 0 0 0 0 0 0 ""                   
          (if (hash-has-key? data-hash "adjust-ca")
              (input-type-number->number (hash-ref data-hash "adjust-ca"))
              0)
          (if (hash-has-key? data-hash "adjust-us")
              (input-type-number->number (hash-ref data-hash "adjust-us"))
              0)
          (if (hash-has-key? data-hash "adjust-intl")
              (input-type-number->number (hash-ref data-hash "adjust-intl"))
              0)
          (if (hash-has-key? data-hash "adjust-fixed")
              (input-type-number->number (hash-ref data-hash "adjust-fixed"))
              0)
          (if (hash-has-key? data-hash "adjust-other")
              (input-type-number->number (hash-ref data-hash "adjust-other"))
              0))]))
|#

;; given a data-hash and the current total value from a the allocation adjust form
;; return a struct roi-line
(define (data-hash->adjustment-roi-line data-hash current-total-line)
  (cond [(null? data-hash) (roi-line "undefined roi" 0 0 0 0 0 0 ""
                            0 0 0 0 0)]
        [else
         (define ca-adjust (input-type-number->number (hash-ref data-hash "adjust-ca")))
         (define us-adjust (input-type-number->number (hash-ref data-hash "adjust-us")))
         (define intl-adjust (input-type-number->number (hash-ref data-hash "adjust-intl")))
         (define fixed-adjust (input-type-number->number (hash-ref data-hash "adjust-fixed")))
         (define other-adjust (input-type-number->number (hash-ref data-hash "adjust-other")))
         (define new-total-value
           (+ (roi-line-value current-total-line) ca-adjust us-adjust intl-adjust fixed-adjust other-adjust))
         (roi-line "Adjusted Allocation Values" 0 0 new-total-value 0 0 0 "what?"                   
                   (+ (roi-line-ca current-total-line) ca-adjust)
                   (+ (roi-line-us current-total-line) us-adjust)
                   (+ (roi-line-intl current-total-line) intl-adjust)
                   (+ (roi-line-fixed current-total-line) fixed-adjust)
                   (+ (roi-line-other current-total-line) other-adjust))]))


(define (allocation-view gnucash-data gnucash-file-path arg-date url allocation-hash [data-hash null])
  ;(with-handlers ([exn:fail? (λ (e) (displayln e)(exception-page (exn-message e)))])
  ;(with-handlers ([exn:fail? (λ (e) (exn-handler e))])
  (let* ([list-alloc-rec (if (null? allocation-hash) '()
                             (alloc-rec-except-target allocation-hash))]
         [master-list-roi (roi-on-date gnucash-data arg-date allocation-hash)]
         [grand-total-line (calc-grand-total-list-account-roi master-list-roi)]
         ; allocation
         [actual-allocation-percent-line (make-allocation-percent grand-total-line)]
         [target-allocation (hash-ref allocation-hash "TARGET")]
         [target-allocation-values
          (add-allocation-to-roi-line grand-total-line target-allocation)]
         [target-allocation-percent (make-roi-line-from-allocation-rec target-allocation)]
         [difference-allocation-percent (subtract-roi-line target-allocation-percent actual-allocation-percent-line)]
         [difference-allocation-value (subtract-roi-line target-allocation-values grand-total-line)]
         ;; adjustment section - pass current grand total to get correct adjustment roi-line
         [adjustment-input-roi-line
          ;; feed an empty roi-line to get just the adjustment amounts
          (data-hash->adjustment-roi-line
           data-hash
           (roi-line "undefined roi" 0 0 0 0 0 0 "" 0 0 0 0 0))]
         [adjustment-roi-line (data-hash->adjustment-roi-line data-hash grand-total-line)]
         [adjusted-allocation-percent
          (begin
            (if (null? data-hash)
                null
                ;(adjust-allocation-percent data-hash actual-allocation-percent-line allocation-hash))]
                (make-allocation-percent (subtract-roi-line target-allocation-values adjustment-roi-line))))]
         [adjusted-allocation-percent-line (make-allocation-percent adjustment-roi-line)]
         [adjusted-difference-allocation-percent
          (subtract-roi-line target-allocation-percent adjusted-allocation-percent-line)]
         ;; boilerplate
         [view-heading (format "Asset Allocation - ~a" arg-date)]
         [page-title (format "~a | GnuCash" view-heading)]
         [main-content-heading view-heading]
         [date-selector (make-date-selector gnucash-data arg-date url)]
         [form-url url]
         [extra-javascript (include-template "static/js/date-selector.js")])

    
    ;(displayln (hash-values allocation-hash))
    ;(print-roi-line target-allocation-percentage)
    ;(print-roi-line actual-allocation-percent-line)    
    (response-200-base-template page-title main-content-heading
                                (include-template "templates/allocation-view.html"))))


;; load allocation data from file
;; allocation-file-path comes from settings.rkt
;; this function looks into the directory where the web-server was run (yes! this is my app)
;; return hash with pairs commodity-id alloc-rec
;; note target allocation is under commodity TARGET (important elsewhere)
(define (get-allocation-hash)
  (define allocation-hash (file->alloc-hash allocation-file-path))
  (if (valid-alloc-hash? allocation-hash)
      allocation-hash
      (let ([message (format "Allocation data from ~a is not valid" allocation-file-path)])
        ; call again to display messages on console
        (valid-alloc-hash? allocation-hash #t)
        (error message))))

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

(define (dashboard-view request)
  (let* ([page-title "Dashboard | GnuCash App"]
        [main-content-heading "Dashboard"]
        [clients (list (list "Smith" "Mark") (list "Simpson" "Lou"))]
        [main-content (include-template "templates/My-Dashboard.html")])
    (response-200-base-template page-title main-content-heading main-content)))

(define (exception-page e)
  (define main-message (format "Caught the following exception: ~a" e))
  (define view-heading (format "ERROR: ~a" e))
  (define page-title "Exception | GnuCash App")
  (define main-content-heading "ERROR")         
  (define main-content main-message)
  (response-200-base-template page-title main-content-heading main-content))

(define (generic-404 request)
  (let ([request-url (url->string (request-uri request))])
     (http-response-404 (format "Cannot dispatch URL: '~a'" request-url))))

;; ----------------
;;  HTML RESPONSES
;; ----------------

(define (response-200-base-template page-title main-content-heading main-content [optional-message ""])
  (let* ([base-css (include-template "static/css/base.css")]
         [bootstrap-js-links (include-template "templates/bootstrap-js-links.txt")]
         [html (include-template "templates/base-template.html")]
         )
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


;; ------------------
;;  TEMPLATE HELPERS
;; ------------------

;; return "d-none" from bootstrap to hide the element
(define (class-for-hide-if-blank text)
  (if (equal? "" text)
      "d-none"
      ""))

;; -------------
;;  ERROR PAGES
;; -------------
(define (app-cannot-start-view error-message)
    (let* ([view-heading "App Cannot Start"]
           [page-title (format "~a | GnuCash" view-heading)]
           [main-content-heading view-heading]
           [error-message error-message])
      (response-200-base-template page-title main-content-heading
                                  (include-template "templates/app-cannot-start-view.html"))))