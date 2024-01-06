#lang racket

#|

MODULE GNUCASH-PARSER

Parse a GnuCash file into classes.
Export a single function "import-gnucash-file" that returns a
single "gnucash-data%" class working as a repository.
|#


(provide import-gnucash-file)

(require
  racket/block
  racket/list
  rackunit
  racket/gui/base)


(require (file "gnucash-objects.rkt"))
         ; link to views will cycle)

(define %TEMPLATE-ROOT-FULLNAME% "Template Root")
(define %ROOT-NAME% "Root Account")


(define HUGE-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define SMALL-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")
(define TRUNCATED-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\TRUNCATED.gnucash")

;; ---------------------------
;;   GNUCASH DATA DEFINITIONS
;; ---------------------------
(define ACCOUNT-START "<gnc:account version=\"2.0.0\">")
(define ACCOUNT-END "</gnc:account>")
(define ACCOUNT-HIDDEN "<slot:key>")
(define ACCOUNT-NAME "<act:name>")
(define ACCOUNT-TYPE "<act:type>")
(define ACCOUNT-GUID "<act:id type=\"guid\">")
(define ACCOUNT-PARENT-ID "<act:parent type=\"guid\">")
(define ACCOUNT-COMMODITY-ID "<cmdty:id>")

(define TRANSACTION-START "<gnc:transaction version=\"2.0.0\">")
(define TRANSACTION-END "</gnc:transaction>")
(define TRANSACTION-DESCRIPTION "<trn:description>")
(define TRANSACTION-DATE-POSTED "<trn:date-posted>")
(define TRANSACTION-ID "<trn:id type=\"guid\">")
(define TRANSACTION-MEMO "<slot:value type=\"string\">")

(define ALL-SPLITS-START "<trn:splits>")
(define ALL-SPLITS-END "</trn:splits>")
(define SPLIT-START "<trn:split>")
(define SPLIT-END "</trn:split>")
(define SPLIT-ID "<split:id type=\"guid\">")
(define SPLIT-MEMO "<split:memo>")
(define SPLIT-ACTION "<split:action>")
(define SPLIT-VALUE "<split:value>")
(define SPLIT-QUANTITY "<split:quantity>")
(define SPLIT-ACCOUNT-ID "<split:account type=\"guid\">")


;(define ALL-PRICES-START "<gnc:pricedb version=\"1\">")
;(define ALL-PRICES-END "</gnc:pricedb>")
(define PRICE-START "<price>")
(define PRICE-END "</price>")
(define PRICE-START-COMMODITY "<price:commodity>")
(define PRICE-END-COMMODITY "</price:commodity>")
(define PRICE-COMMODITY-ID "<cmdty:id>")
(define PRICE-VALUE "<price:value>")
(define PRICE-DATE "<ts:date>")

(define COMMODITY-START "<gnc:commodity version=\"2.0.0\">")
(define COMMODITY-END "</gnc:commodity>")
(define COMMODITY-ID "<cmdty:id>")
(define COMMODITY-NAME "<cmdty:name>")
(define COMMODITY-FRACTION "<cmdty:fraction>")
(define COMMODITY-SPACE "<cmdty:space>")



  
;;-------------------
;; IMPORT FUNCTIONS
;;------------------

;; read a commodity from the file and return it
(define (import-commodity in)
  (define commodity (make-object commodity%))
  (for ([raw-line (in-lines in)])
    #:break (string-contains? raw-line COMMODITY-END)
    (define line (string-trim raw-line))
    (define value (element-value line))
    (cond
      [(string-prefix? line COMMODITY-NAME)
       (send commodity set-name! value)]
      [(string-prefix? line COMMODITY-ID)
       (send commodity set-id! value)]
      [(string-contains? line COMMODITY-FRACTION)
       (send commodity set-fraction! value)]
      [(string-prefix? line COMMODITY-SPACE)
       (send commodity set-space! value)]))
  commodity)

;; read an account from the file and return it
(define (import-account in)
  (define account (make-object account%))
  (for ([raw-line (in-lines in)])
    #:break (string-contains? raw-line ACCOUNT-END)
    (define line (string-trim raw-line))
    (define value (element-value line))
    (cond
      [(string-prefix? line ACCOUNT-NAME)
       (send account set-name! value)]
      ; ACCOUNT-HIDDEN in only found if account is hidden
      ; default in account% is  visible
      [(string-prefix? line ACCOUNT-HIDDEN)
       (send account set-visible! #f)]
      [(string-prefix? line ACCOUNT-PARENT-ID)
       (send account set-parent-id! value)]
      [(string-contains? line ACCOUNT-COMMODITY-ID)
       (send account set-commodity-id! value)]
      [(string-contains? line ACCOUNT-GUID)
       (send account set-id! value)]
      [(string-prefix? line ACCOUNT-TYPE)
       (send account set-type! value)]))
  account)

;; read a single split from the file and return it
(define (import-single-split in)
  (define split (make-object split%))
  (for ([raw-line (in-lines in)])
    #:break (string-contains? raw-line SPLIT-END)
    (define line (string-trim raw-line))
    (define value (element-value line))
    (cond
      [(string-prefix? line SPLIT-ID)
           (send split set-id! value)]
      [(string-prefix? line SPLIT-VALUE)
       (send split set-value! (string->number value))]
      [(string-contains? line SPLIT-QUANTITY)
       (send split set-quantity! (string->number value))]
      [(string-contains? line SPLIT-MEMO)
       (send split set-memo! value)]
      [(string-contains? line SPLIT-ACTION)
       (send split set-action! value)]
      [(string-prefix? line SPLIT-ACCOUNT-ID)
       (send split set-account-id! value)]))
  split)

;; extract the commodity-id from one of the following lines
;; assume you were just on line PRICE-START-COMMODITY
;; loop to find PRICE-COMMODITY-ID
;; if you reach PRICE-END-COMMODITY, there was an error
(define (import-commodity-id in)
  (let loop ([line (next-line in)])
    (cond
      ; found it!
      [(string-contains? line PRICE-COMMODITY-ID) (element-value line)]
      ; oops! didn't find it
      [(equal? line PRICE-END-COMMODITY)
       (error "CAN'T FIND PRICE'S COMMODITY-ID. CHECK THE INPUT FILE!")]
      ; keep looking
      [else (loop (next-line in))])))
          
 
;; import a single price 
(define (import-price in)
  (define price (make-vanilla-price))
  (for ([raw-line (in-lines in)])
    #:break (string-contains? raw-line PRICE-END)
    (define line (string-trim raw-line))
    (define value (element-value line))
    (cond
      [(string-prefix? line PRICE-DATE)
       (send price set-date! (substring value 0 10))]
      [(string-prefix? line PRICE-VALUE)
       (send price set-value! (string->number value))]
      [(string-contains? line PRICE-START-COMMODITY)
       (send price set-commodity-id! (import-commodity-id in))]))
  price)


;; loop throught splits section and add individual splits to collection
(define (import-all-splits in)
  ; simplify with for/list
  (for/list ([line (in-lines in)])
    #:break (string-contains? line ALL-SPLITS-END)
    (import-single-split in)))
    

;; read a transaction from the file and return it
(define (import-transaction in)
  (define transaction (make-object transaction%))  
  (for ([raw-line (in-lines in)])
    #:break (equal? raw-line TRANSACTION-END)
    (define line (string-trim raw-line))
    (define value (element-value line))
    (cond
      [(string-prefix? line TRANSACTION-DESCRIPTION)           
       (send transaction set-description! value)]
      [(string-prefix? line TRANSACTION-MEMO)
       (send transaction set-memo! value)]
      [(string-prefix? line TRANSACTION-DATE-POSTED)
       (send transaction set-date-posted!
             (substring (next-line-element-value in) 0 10))]
      [(string-contains? line TRANSACTION-ID)
       (send transaction set-id! value)]
      [(string-prefix? line ALL-SPLITS-START)
       (send transaction set-splits! (import-all-splits in))]))
  transaction)
    
    

;;-----------------------------
;;    MAIN READER AND DISPATCH
;;-----------------------------

; the GnuCash reader loops a GnuCash to the EOF and sends lines to the dispatch function
(define (import-gnucash-file path)
  (printf "Importing GnuCash data from ~a~%" path)
  (let ([gnucash (make-object gnucash-data%)])
   (send gnucash set-file-path! path)
   (call-with-input-file path    
    (lambda (in)
      (for ([line (in-lines in)])
        ; note: dispatch also advances the reading cursor in the file
        (dispatch-line gnucash (string-trim line) in))))
      
    (build-metadata gnucash)
    ; clear temp list of transactions to (maybe) free resources
    ; transactions are stored in accounts
    (send gnucash clear-all-transactions)
    gnucash))

;; detect start of object definition and route to matching function
;; arg line: string current line (CR/LF and left-padding removed)
;; arg in: the input port
(define (dispatch-line gnucash-data line in)
  (cond
    [(equal? line ACCOUNT-START) (send gnucash-data add-account! (import-account in))]
    [(equal? line COMMODITY-START) (send gnucash-data add-commodity! (import-commodity in))]        
    [(equal? line PRICE-START) (send gnucash-data add-price! (import-price in))]        
    [(equal? line TRANSACTION-START) (send gnucash-data add-transaction! (import-transaction in))]))


;; -------------------------------------------------
;;   BUILD METADATA
;;   Links and references not explicit in raw data
;; -------------------------------------------------

;; main function for building metadata then clearing global transaction list
;; MODIFIES gnucash-data
;; no return(define (build-metadata gnucash-data)
(define (build-metadata gnucash-data)
  (account-metadata gnucash-data)
  (link-split-account-transaction gnucash-data)
  (send gnucash-data clear-all-transactions)
  (send gnucash-data sort-prices)
  (send gnucash-data sort-child-accounts)
  (send gnucash-data sort-account-transactions)
  )


;; add matching transaction to account, account to split (not just id)
;; template transactions exit for accounts that were deleted - skip them
(define (link-split-account-transaction gnucash-data)
  (for ([transaction (send gnucash-data get-transactions)])
    (for ([split (in-list (send transaction get-splits))])
      (let* ([account-id (send split get-account-id)]
             [account (send gnucash-data account-by-id account-id)])
        (cond [(not (void? account))
               (block
                (send split set-account! account)
                (send account add-transaction! transaction))])))))
             

;; build metadata for accounts
;; eg. link to parent object using id from file,
;; build full name with all parent names,
;; store accounts by fullname
;; hide template accounts
;; MODIFIES gnucash-date
;; no return
;; yeah, this could be split into different functions...
(define (account-metadata gnucash-data)
  ;; we define it here because we can't search by full-name yet since metadata is not built
  ;; after this, search by fullname if needed?
  (define accounts (send gnucash-data all-accounts))
  (define template-root-id
    (let ([act (send gnucash-data account-by-fullname %TEMPLATE-ROOT-FULLNAME%)])
               (if (null? act) null (send act get-id))))                                          
    ; link to parent object
  (for ([act (in-list accounts)])
    (define parent-id (send act get-parent-id))
    (when (not (equal? "" parent-id))
           ; set-parent! also adds account as child to that parent
      (send act set-parent! (send gnucash-data account-by-id parent-id))))
  ; remove templates; yes we loop again
  ; identify root
  ; build full name with recursive function
  (for ([act (in-list (send gnucash-data all-accounts))])
    (let ([account-name (send act get-name)]
          [account-id (send act get-id)] 
          [parent-id (send act get-parent-id)])
      (cond [(equal? account-name %ROOT-NAME%) (send gnucash-data set-root-account! act)]
            [(or (equal? account-name %TEMPLATE-ROOT-FULLNAME%) (equal? template-root-id parent-id))
             (send gnucash-data remove-account act)]
            [else (build-fullname act)])))
  
  )

;; prepend the parent name to the fullname of the account
;; recurse up the parent tree
(define (build-fullname account)
  ; start by setting fullname to account name
  (send account set-fullname! (send account get-name))
  ; recursively read up the chain or parents
  (let loop ([parent (send account get-parent)])
    (when (not (or (void? parent) (equal? %ROOT-NAME% (send parent get-name))))
      (define fullname (send account get-fullname))
      (define parent-name (send parent get-name))
      (define new-fullname (string-append parent-name ":" fullname))
      (send account set-fullname! new-fullname)
      (loop (send parent get-parent)))))


;;------------------
;;  IMPORT HELPERS
;;------------------

; is the trimmed string equal to the value?
; "   bob" trimmed equals "bob"
(define (trim-equal? str value)
  (equal? (string-trim str) value))

;; given a port, return the next-line, trimmed
;; NOTE: some account description span many lines;
;;       one may need to fix that here (loop and append until line ends in >)
(define (next-line in)
  (let ([line (read-line in 'any)])
    (if (eof-object? line)
        line
        (string-trim line))))

;; return the location of a character in a string, -1 if not found
;; char must be passed as a character, like "a" is #\a
(define (char-position str char)
  (define len (string-length str))
  (let loop ([n 0])
    (cond
      [(equal? n len) -1]
      [(equal? char (string-ref str n)) n]
      [else (loop (add1 n))])))
   
;; extract inner string from xml-like element
;; fix: some values in account description can be multiline
;;      so, some lines don't have a start and finish
;;      FIX is skip lines that don't start with '<'
(define (element-value in-str)
  (define str (string-trim in-str))
  (cond
    [(not (string-prefix? str "<")) ""]
    [else 
     (define index> (char-position str #\>))
     (cond
       [(not index>) ""]
       [else
        ;; skip the first < and add 1 to index later
        (define index< (char-position (substring str 1) #\<))
        (if (negative? index<)
            ""
            (substring str (+ 1 index>) (add1 index<)))])]))

;; return the value on the next line
;; example:
;;   <date> <=== you are here
;;     <iso-date>2012-11-12</iso-date> <==== you want this line
;;   </date>
;; arg in is the file handle
(define (next-line-element-value in)
    (element-value (next-line in)))


;; --------------
;;   UNIT TESTS
;; --------------

(check-eq? (char-position "allo" #\l) 1)
(check-eq? (char-position "allo" #\x) -1)
(check-equal? (element-value "<name>bob<df") "bob")
(check-equal? (element-value "<name><df") "")
(check-equal? (element-value "name") "")
(check-equal? (element-value "name><df") "") ;; element does not start with <
(check-equal? (element-value "<name>asdfasdfa") "") ;; element doesn't end on same line: skip it
(check-equal? (element-value "asdfafda</name>") "") ;; element ends here, but we don't have the start, so skip it
