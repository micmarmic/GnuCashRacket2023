#lang racket
(require rackunit)
(require racket/block)

(provide (all-defined-out))


;; -------------------------------
;; This module contains the objects we use to represent the GnuCash data
;; -------------------------------

(define TYPE-ASSET "ASSET")
(define TYPE-MUTUAL "MUTUAL")
(define TYPE-STOCK "STOCK")
(define TYPE-EXPENSE "EXPENSE")

;; --------------------------------------------
;; Class gnucash-data%: repository for all data
;; --------------------------------------------

;; Class gnucash-data% is repository for all data
;; Instantiated and populated by functions in gnucash-parser.rkt
(define gnucash-data%
  (class object%
    (super-new)
    (field [root-account void] [accounts-by-fullname (make-hash)] [accounts-by-id (make-hash)]
           [commodities-by-name (make-hash)]
           [prices-by-cmdty-id (make-hash)]
           [min-trans-year 3000]
           [max-trans-year 0]
           [transactions '()];[transactions-by-id (make-hash)]
           [file-path ""])

    ; data file
    (define/public (set-file-path! arg-path) (set! file-path arg-path))
    (define/public (get-file-path) file-path)

    ;
    ; Transactions
    ; 
    (define/public (num-transactions) (length transactions))
    (define/public (get-transactions) transactions)
    
    (define/public (add-transaction! transaction)
        (if (void? transaction)
            (error "[add-transaction!] transaction is void!")
            (let ([year (string->number (substring (send transaction get-date) 0 4))])
              (when (< year min-trans-year)
                (set! min-trans-year year))
              (when (> year max-trans-year)
                (set! max-trans-year year))
              (set! transactions (append transactions (list transaction))))))

    (define/public (get-min-trans-year)
      min-trans-year)

    (define/public (get-max-trans-year)
      max-trans-year)

    ; after metadata is loaded, accounts access transactions, not the main repo
    (define/public (clear-all-transactions)
      ;(set! transactions-by-id (void)))
      (set! transactions '()))

    ;
    ; Commodities
    ;
    (define/public (num-commodities) (length (hash-values commodities-by-name)))

    (define/public (add-commodity! commodity)
      (hash-set*! commodities-by-name (send commodity get-id) commodity))

    (define/public (all-commodities-by-name)
      (sort (hash-values commodities-by-name) commodity-name<?))
    
    ;
    ; Root
    ;
    (define/public (get-root-account) root-account)
    (define/public (set-root-account! account) (set! root-account account))


    ;
    ; Accounts
    ; 
    (define/public (num-accounts) (length (hash-values accounts-by-id)))

    (define/public (add-account! account)
      (if (void? account)
          (error "Account is void!")
          (block
           (hash-set*! accounts-by-fullname (send account get-fullname) account)
           (hash-set*! accounts-by-id (send account get-id) account))))

    ; return null if not found
    (define/public (account-by-fullname arg-fullname)
      (let ([found-list (filter (lambda (act) (equal? arg-fullname (send act get-fullname))) (hash-values accounts-by-id))])
        (if (empty? found-list)
            ;(error (format "can't find account with fullname ~a~%" arg-fullname))
            null
            (car found-list))))

    (define/public (account-by-id id)
      (if (hash-has-key? accounts-by-id id)
          (hash-ref accounts-by-id id)
          (void)))
        
    (define/public (sort-account-transactions)
      (for ([account (hash-values accounts-by-id)])
        (send account sort-transactions)))

   
    (define/public accounts-sorted-by-fullname
      (lambda ([show-hidden #f])
        (let ([filtered-accounts
               (if show-hidden
                   (get-accounts-hidden-or-not #t)
                   (get-accounts-hidden-or-not #f))])
          (sort filtered-accounts  account-fullname<?))))
    
    (define/public (all-accounts)
      (hash-values accounts-by-id))

    (define/public (holding-accounts)
      (filter (lambda (act) (send act is-holding?)) (send this all-accounts)))      

    (define (get-accounts-hidden-or-not show-hidden)
      (let* ([all-acts (hash-values accounts-by-id)]
             [filtered-accounts (if show-hidden
                                   all-acts
                                   (filter (lambda (act) (not (send act is-visible?))) all-acts))])
        filtered-accounts))

    (private get-accounts-hidden-or-not)
    
    (define/public (remove-account account)
      (hash-remove! accounts-by-fullname (send account get-fullname))
      (hash-remove! accounts-by-id (send account get-id)))

    ;
    ; Prices
    ;
    (define/public (add-price! price)
      (let* ([cmdty-id (send price get-commodity-id)]
             [price-list (if (hash-has-key? prices-by-cmdty-id cmdty-id)
                             (hash-ref  prices-by-cmdty-id cmdty-id)
                             '())] ; make new price list
             [new-list (append price-list (list price))])
        (hash-set*! prices-by-cmdty-id cmdty-id new-list)))
    
    (define/public (get-prices-by-cmdty-id) prices-by-cmdty-id)

    (define/public (num-price-lists) (hash-count prices-by-cmdty-id))

    ; return a list of prices
    (define/public (price-list-for-cmdty-id id)
      (if (hash-has-key? prices-by-cmdty-id id) 
          (hash-ref prices-by-cmdty-id id)
          (block 
           (printf "INFO: no prices for ~a~%" id)
           null)))
          

    ; sort prices chronologically
    (define/public (sort-prices)
      (for ([cmdty-id (hash-keys prices-by-cmdty-id)])
        (let ([price-list (hash-ref prices-by-cmdty-id cmdty-id)])
          (hash-set*! prices-by-cmdty-id cmdty-id
                    (sort price-list price-date<?)))))
    

    (define/public (sort-child-accounts)
      (for ([account (hash-values accounts-by-id)])
        (send account sort-children)))

    
  
    )) ; end class definition 


;; -----------------------------------
;; Functions with gnucash-data% as arg
;; -----------------------------------
   
(define (display-all-accounts gnucash)
  (for ([act (send gnucash accounts-sorted-by-name)])
    (if (void? act)
        (display "ERROR! account is void!!!")
        (displayln (send act as-string)))))

(define (display-transactions-all-splits gnucash)
  (for ([tran (send gnucash get-list-transactions)])
    (if (void? tran)
        (display "ERROR! transaction is void!!!")
        (displayln (send tran as-string-all-splits)))))

(define (display-transactions-single-line gnucash)
  (for ([tran (send gnucash get-list-transactions)])
    (if (void? tran)
        (display "ERROR! transaction is void!!!")
        (displayln (send tran as-string-single-line)))))


(define (print-overview gnucash)
  (displayln "")
  (displayln "----------------")
  (displayln "    OVERVIEW")
  (displayln "----------------")
  (printf "File path: ~a~%" (send gnucash get-file-path))
  (printf "Number of accounts: ~a~%" (send gnucash num-accounts))
  (printf "Number of commodities: ~a~%" (send gnucash num-commodities))
  (printf "Number of commodities with prices: ~a~%" (send gnucash num-price-lists))
  (define num-prices (foldr (lambda (lst-prices result) (+ result (length lst-prices)))
                      0
                      (hash-values (send gnucash get-prices-by-cmdty-id))))
  (printf "Number of prices: ~a~%" num-prices)
  (displayln ""))

;; ------------
;; CLASS split%
;; ------------

;; A split from the GnuCash file
(define split%
  (class object%
    (super-new)
    (field [id ""] [account-id ""] [action ""] [account (void)] [value 0.0] [quantity 0.0] [memo ""] [splits '()])

    (define/public (set-id! arg-id) (set! id arg-id))
    (define/public (get-id) id)

    (define/public (set-account-id! arg-account-id) (set! account-id arg-account-id))
    (define/public (get-account-id) account-id)

    (define/public (set-action! arg-action) (set! action arg-action))
    (define/public (get-action) action)
    
    
    (define/public (set-account! arg-account) (set! account arg-account))
    (define/public (get-account) account)
    (define/public (get-account-name) (send account get-name))
    
    (define/public (set-value! arg-value) (set! value arg-value))
    (define/public (get-value) value)

    (define/public (set-quantity! arg-quantity)
       (set! quantity arg-quantity))
    (define/public (get-quantity) quantity)

    (define/public (get-memo) memo)
    (define/public (set-memo! arg-memo) (set! memo arg-memo))
    
    (define/public (as-string)
      (let ([account-str (if (void? account) id (send account get-name))])
        (format "Memo:~a Account: ~a Quantity: ~a Value:~a"
                memo
                account-str
                (real->decimal-string quantity)
                (real->decimal-string value)
                )))
    ))

;; ------------------
;; CLASS transaction%
;; ------------------

;; An transaction from the GnuCash file and its splits
(define transaction%
  (class object%
    (super-new)
    (field [id ""] [description ""] [date-posted ""] [splits (void)] [memo ""])

    ;; setters-getters
    (define/public (set-id! arg-id) (set! id arg-id))
    (define/public (set-description! arg-description) (set! description arg-description))
    (define/public (set-date-posted! arg-date-posted) (set! date-posted arg-date-posted))

    (define/public (get-id) id)
    (define/public (get-description) description)
    (define/public (get-date) date-posted)

    (define/public (num-splits) (length splits))
    (define/public (get-splits) splits)
    (define/public (set-splits! list-splits) (set! splits list-splits))

    (define/public (get-memo) memo)
    (define/public (set-memo! arg-memo) (set! memo arg-memo))
    
    
    (define/public (as-string-all-splits)
      (let* ([result (format "Date:~a Desc:~a Id: ~a~ (~a splits)%"
                             date-posted description id (num-splits))])
        (if (void? splits)
            (set! result (string-append result "\n" "NO SPLITS"))
            ;;(set! result (string-append result "   \n" "asplit")))
            (for ([split splits])
              (set! result (string-append result "\n   " (send split as-string)))))
        result))
    
    (define/public (as-string)
      (format "Date:~a Desc:~a Id: ~a~ (~a splits)"
                             date-posted description id (num-splits)))
))
    

;; ----------------
;; CLASS commodity%
;; ----------------

;; A commodity from the GnuCash file
(define commodity%
  (class object%
    (super-new)
    (field [id ""] [name ""] [space ""] [fraction ""])

    (define/public (set-name! arg-name) (set! name arg-name))
    (define/public (set-space! arg-space) (set! space arg-space))
    (define/public (set-fraction! arg-fraction) (set! fraction arg-fraction))
    (define/public (set-id! arg-id) (set! id arg-id))
    
    (define/public (get-id) id)
    (define/public (get-name) name)
    (define/public (get-space) space)
    (define/public (get-fraction) fraction)
    

    (define/public (as-string)
      (format "Name:~a Space: ~a Fraction:~a Id: ~a" name space fraction id))
  ))
    

;; --------------------------------
;; Functions with commodity% as arg
;; --------------------------------

;; display all commodities
;; in: gnucash-data
;; out: standard output
(define (display-all-commodities gnucash-data)
  (block
   (printf "COMMODITIES ~a~%" (send gnucash-data num-commodities))   
   (for ([commodity (send gnucash-data all-commodities-by-name)])
    (displayln (send commodity as-string)))))

;; ----------------
;; CLASS price%
;; ----------------

;; An price from the GnuCash file
(define price%
  (class object%
    (init init-commo)
    (init init-date)
    (init init-value)

    (define commodity-id init-commo)
    (define date init-date)
    (define value init-value)
    
    (super-new)
    ;(field [commodity-id ""] [date "NO DATE FOUND"] [value 0.0])

    (define/public (set-commodity-id! arg-id) (set! commodity-id arg-id))
    (define/public (set-date! arg-date) (set! date arg-date))
    (define/public (set-value! arg-value) (set! value arg-value))

    (define/public (get-commodity-id) commodity-id)
    (define/public (get-date) date)
    (define/public (get-value) value)

    (define/public (as-string)
      (format "commodity-id: ~a date: ~a value: ~a" commodity-id date (real->decimal-string value)))
    
))

(define (make-vanilla-price)
  (make-object price% "UNDEFINED" "1950-12-31" 0))

; <cmdty:id>
; <ts:date>2023-08-31 10:59:00 +0000</ts:date>
; <price:value>898/25</price:value>

    
;; ----------------
;; CLASS account%
;; ----------------

;; An account from the GnuCash file
(define account%
  (class object%
    (super-new)
    (field [name ""] [id ""] [parent-id ""] [type ""] [sort-name ""] [parent (void)] [children '()]
            [fullname ""] [transactions '()] [commodity-id ""] [visible #t])

    ;
    ; BASE ATTRIBUTES
    ; 

    (define/public (get-name) name)
    (define/public (set-name! arg-name)
      (set! name arg-name))

    (define/public (set-fullname! arg-fullname)
      (let ([new-sort-name (lcase-no-accents arg-fullname)])
        (set! fullname arg-fullname)
        (set! sort-name new-sort-name)))
    (define/public (get-fullname) fullname)

    (define/public (get-sort-name) sort-name)

    (define/public (set-id! arg-id) (set! id arg-id))
    (define/public (get-id) id)

    (define/public (set-type! arg-type) (set! type arg-type))
    (define/public (get-type) type)
    (define/public (is-investment?) (list? (member type (list TYPE-MUTUAL TYPE-STOCK))))

   
    (define/public (set-commodity-id! arg-commodity-id)
      (set! commodity-id arg-commodity-id))
    (define/public (get-commodity-id)
      commodity-id)

    ; a holding account has children of investment types
    (define/public (is-holding?)
      (ormap (lambda (act) (send act is-investment?)) children))

    (define/public (is-visible?)
      visible)
    (define/public (set-visible! t-or-f)
      (set! visible t-or-f))

    ;
    ; PARENT-CHILDREN
    ; 

    (define/public (get-children) children)
    (define/public (add-child! account)
      (set! children (append children (list account))))

    (define/public (sort-children)
      (set! children (sort children account-fullname<?)))
    
    (define/public (set-parent-id! arg-parent-id) (set! parent-id arg-parent-id))
    (define/public (get-parent-id) parent-id)
    ;; set the parent, and add this to the parent's child list
    (define/public (set-parent! arg-parent)
      (set! parent arg-parent)
      (send arg-parent add-child! this))
    (define/public (get-parent) parent)

    ; how deep in the tree is this account?
    ; "expenses" -> 1; "expenses:rent" -> 2
    (define/public (tree-depth)
      (length (string-split fullname ":")))
      
    ;
    ; TRANSACTIONS
    ;
    (define/public (add-transaction! transaction)
      ; don't add the same transaction twice: some transactions can refer to the same account twice in splits
      ; like an investement with buy @ price1 and @ price2
      (when (not (member transaction transactions))        
        ;(hash-set*! transactions-by-id (send transaction get-id) transaction)))
         (set! transactions (append transactions (list transaction)))))

    (define/public (sort-transactions)
      (set! transactions (sort transactions transaction-date<?)))

    (define/public (num-transactions) (length transactions))
    (define/public (transactions-sorted-by-date)
      ;; disable sort for now: are transactions in correct order in file?
      transactions)
    
      ;;(sort transactions transaction-dates))
    (define/public (get-transactions)
      transactions)

    ;;
    ;; DISPLAY
    ;;
    (define/public (as-string)
      (let ([parent-name "TODO"])
        (if (void? parent)
            (set! parent-name "NO PARENT")
            (set! parent-name (send parent get-name)))        
        (format "Fullname: ~a Type: ~a Id: ~a] Commodity-id: ~a Parent name: '~a'" fullname type id commodity-id parent-name)))  
))

;; -----------------------------------
;; Functions with account% as arg
;; -----------------------------------
(define (display-all-transactions-all-splits-in-account account)
  (block
   (printf "DEBUG ACCOUNT: ~a~%" (send account get-name))
   (printf "NUM TRANSACTIONS: ~a~%" (send account num-transactions))
   (printf "FIRST TRANS DATE: ~a~%" (send (first (send account get-transactions)) get-date))
   (for ([transaction (send account get-transactions)])
    (displayln (send transaction as-string-all-splits)))))

;;-----------------------
;;  HELPERS FOR SORTING
;;  * use 'account get-sort-name' for sorting *
;;-----------------------

;; list of chars to replace in lcase-no-accents
(define ACCENTED "àâäçéèêëìîïòôöùûü")
(define ACCENT-REMOVED "aaaceeeeiiiooouuu")

(define (compare-strings-without-accents a b)
  (let ([a-fixed (lcase-no-accents a)]
        [v-fixed (lcase-no-accents b)])
    (string<? a b)))

(define (price-date<? p1 p2)
  (string<? (send p1 get-date) (send p2 get-date)))

(define (account-fullname<? a1 a2)
  (string<? (send a1 get-sort-name) (send a2 get-sort-name)))

(define (commodity-name<? a1 a2)
  (string<? (lcase-no-accents (send a1 get-name)) (lcase-no-accents (send a2 get-name))))


(define (transaction-date<? t1 t2)
  (string<? (send t1 get-date) (send t2 get-date)))

  
;; replace accented chars with non-accented equiv and lower the case
(define (lcase-no-accents str)
  (if (equal? str "")
      ""
      (let ([newstr (string-downcase str)])
        (for ([i (in-range 0 (string-length ACCENTED))])
          (set! newstr (string-replace newstr
                                       (~a (string-ref ACCENTED i))
                                       (~a (string-ref ACCENT-REMOVED i)))))
        newstr)))
  


;; DEBUGGING CODE
#|
(define myrepo (make-object gnucash-data%))
(define a1 (make-object account%))
(send a1 set-name! "Aleve")
(define a2 (make-object account%))
(send a2 set-name! "Brie")
(define a3 (make-object account%))
(send a3 set-name! "Fory")
(define a4 (make-object account%))
(send a4 set-name! "Étude")

(send myrepo add-account a1)
(send myrepo add-account a2)
(send myrepo add-account a3)
(send myrepo add-account a4)
(send myrepo display-all-accounts)
;(printf "num accounts: ~a~%" (send myrepo num-accounts))
|#

;; -----------
;; UNIT TEST
;; -----------

(check-equal? (lcase-no-accents ACCENTED) ACCENT-REMOVED)
(check-equal? (string-length ACCENTED) (string-length ACCENT-REMOVED))

(check-equal? (lcase-no-accents "béb") "beb")
(check-equal? (lcase-no-accents "Michel rembourse dépenses") "michel rembourse depenses")