#lang racket

(provide import-gnucash-file)

(require
  racket/block
  racket/list
  rackunit
  racket/gui/base)

(require "gnucash-objects.rkt")

(define %TEMPLATE-ROOT-FULLNAME% "Template Root")
(define %ROOT-NAME% "Root Account")

 ;(struct-out test-struct))

#|
Parse a GnuCash file into data structures.
Export the data structures individually instead of packaging them in a
main repo object.
|#

(define HUGE-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define SMALL-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")
(define TRUNCATED-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\TRUNCATED.gnucash")

;; -------------------
;;   OBJECT MARKERS
;; -------------------
(define ACCOUNT-START "<gnc:account version=\"2.0.0\">")
(define ACCOUNT-END "</gnc:account>")
(define ACCOUNT-NAME "<act:name>")
(define ACCOUNT-TYPE "<act:type>")
(define ACCOUNT-GUID "<act:id type=\"guid\">")
(define ACCOUNT-PARENT-ID "<act:parent type=\"guid\">")

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
(define SPLIT-VALUE "<split:value>")
(define SPLIT-QUANTITY "<split:quantity>")
(define SPLIT-ACCOUNT-ID "<split:account type=\"guid\">")


(define ALL-PRICES-START "<gnc:pricedb version=\"1\">")
(define ALL-PRICES-END "</gnc:pricedb>")
(define PRICE-START "<price>")
(define PRICE-END "</price>")
(define PRICE-START-COMMODITY "<price:commodity>")
(define PRICE-END-COMMODITY "</price:commodity>")
;; after commodity find commodity-id
(define PRICE-COMMODITY-ID "<cmdty:id>")
(define PRICE-VALUE "<price:value>")
(define PRICE-DATE "<ts:date>")

(define COMMODITY-START "<gnc:commodity version=\"2.0.0\">")
(define COMMODITY-END "</gnc:commodity>")
(define COMMODITY-ID "<cmdty:id>")
(define COMMODITY-NAME "<cmdty:name>")
(define COMMODITY-FRACTION "<cmdty:fraction>")
(define COMMODITY-SPACE "<cmdty:space>")


;;------------------
;;  IMPORT HELPERS
;;------------------
 
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
  (let ([len (string-length str)])
    (let loop ([n 0])
      (cond
         [(equal? n len) -1]
         [(equal? char (string-ref str n)) n]
         [else (loop (add1 n))]))))

;; extract inner string from xml-like element
;; fix: some values in account description can be multiline
;;      so, some lines don't have a start and finish
;;      FIX is skip lines that don't start with '<'
(define (element-value str)
  (if (not (string-prefix? str "<"))
      ""
      (let ([index> (char-position str #\>)])
        (if (negative? index>)
            ""
            ;; skip the first < and add 1 to index later
            (let ([index< (char-position (substring str 1) #\<)])
              (if (negative? index<)
                  ""
                  (substring str (+ 1 index>) (add1 index<))))))))

;; the required values is on the next line; skip one line after
;; example:
;;   <date>
;;     <iso-date>2012-11-12</iso-date>
;;   </date>
;; arg in is what is needed; don't care about the value of the starting line like <date>
(define (next-line-element-value in)
  (let ([line (next-line in)])
    (element-value line)))
  
;;-------------------
;; IMPORT FUNCTIONS
;;------------------

;; read a commodity from the file and return it
(define (import-commodity in)
  (let ([commodity (make-object commodity%)])
    ;(displayln (length *accounts*))
    (let act-loop ([line (next-line in)])
      (if (equal? line COMMODITY-END)
          commodity
          (block
           (let ([value (element-value line)])
             (cond
               [(string-prefix? line COMMODITY-NAME)
                (send commodity set-name! value)]
               [(string-prefix? line COMMODITY-ID)
                (send commodity set-id! value)]
               [(string-contains? line COMMODITY-FRACTION)
                (send commodity set-fraction! value)]
               [(string-prefix? line COMMODITY-SPACE)
                (send commodity set-space! value)]))
          (act-loop (next-line in)))))    
    commodity))

;; read an account from the file and return it
(define (import-account in)
  (let ([account (make-object account%)])
    ;(displayln (length *accounts*))
    (let act-loop ([line (next-line in)])
      (if (equal? line ACCOUNT-END)
          account
          (block
           (let ([value (element-value line)])
             (cond
               [(string-prefix? line ACCOUNT-NAME)
                (send account set-name! value)]
               [(string-prefix? line ACCOUNT-PARENT-ID)
                (send account set-parent-id! value)]
               [(string-contains? line ACCOUNT-GUID)
                (send account set-id! value)]
               [(string-prefix? line ACCOUNT-TYPE)
                (send account set-type! value)]))
          (act-loop (next-line in)))))    
    account))

;; read a single split from the file and return it
(define (import-single-split in)
  (let ([split (make-object split%)])
    (let split-loop ([line (next-line in)])
      (if (equal? line SPLIT-END)
          split
          (block
           (let ([value (element-value line)])
             (cond
               [(string-prefix? line SPLIT-ID)
                (send split set-id! value)]
               [(string-prefix? line SPLIT-VALUE)
                (send split set-value! (string->number value))]
               [(string-contains? line SPLIT-QUANTITY)
                 (send split set-quantity! (string->number value))]
               [(string-contains? line SPLIT-MEMO)
                 (send split set-memo! value)]
               [(string-prefix? line SPLIT-ACCOUNT-ID)
                (send split set-account-id! value)]))
           (split-loop (next-line in)))))
    ;;(printf "IMPORTED split:: ~a~%" (send split as-string))
    split))

#|
(define ALL-PRICES-START "<gnc:pricedb version="1">")
(define ALL-PRICES-END "</gnc:pricedb>")
(define PRICE-START "<price>")
(define PRICE-END "</price>")
(define PRICE-COMMODITY "<price:commodity>")
;; after commodity find commodity-id
(define PRICE-COMMODITY-ID "<cmdty:id>")
(define PRICE-VALUE "<price:value>")
(define PRICE-DATE "<ts:date>")
|#

;; extract the commodity-id from one of the following lines
;; assume you were just on line PRICE-START-COMMODITY
;; loop to find PRICE-COMMODITY-ID
;; if you reach PRICE-END-COMMODITY, there was an error
(define (import-commodity-id in)
  (let loop ([line (next-line in)])
    ;(printf "line ~a~%" line)
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
  (let ([price (make-object price%)])
    (let loop ([line (next-line in)])
      (if (equal? line PRICE-END)
          price
          (let ([value (element-value line)])
            (cond
              [(string-prefix? line PRICE-DATE)
               (send price set-date! (substring value 0 10))]
              [(string-prefix? line PRICE-VALUE)
               (send price set-value! (string->number value))]
              [(string-contains? line PRICE-START-COMMODITY)
               (send price set-commodity-id! (import-commodity-id in))])
            (loop (next-line in)))))
    ;(printf "~a~%" (send price as-string))
    price))



;; import individual splits from split section in file
(define (import-all-splits in)
  ;; on entry, we just read the start of all splits 
  ;; the next line is the start of single split, and we'll
  ;; be handled/ignored by the import-single-split function
  (let ([splits '()])
    (let split-loop ([line (next-line in)])
      (if (equal? line ALL-SPLITS-END)
          splits
          (block
           ;(displayln "--- IMPORT A SPLIT:")
           (set! splits (cons (import-single-split in) splits))
           (split-loop (next-line in)))))
    splits))
    

;; read a transaction from the file and return it
(define (import-transaction in)
  (let ([transaction (make-object transaction%)])
    (let tran-loop ([line (next-line in)])
      (if (equal? line TRANSACTION-END)
          transaction
          (block
           (let ([value (element-value line)])
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
           (tran-loop (next-line in)))))
    ;(printf "IMPORTED transaction ~a~%" (send transaction get-id))
    transaction))

;;-----------------------------
;;    MAIN READER AND DISPATCH
;;-----------------------------

; the GnuCash reader loops a GnuCash to the EOF and sends lines to the dispatch function
(define (import-gnucash-file path)
  (let ([gnucash (make-object gnucash-data%)])
   (send gnucash set-file-path! path)
   (call-with-input-file path    
    (lambda (in)
      (let loop ([line (next-line in)])
        (cond
          [(eof-object? line) '()]
          [ else
            (block
              (dispatch-line gnucash line in)
              (loop (next-line in)))]))))
    ;(printf "IMPORTED ~a ACCOUNTS before purging templates~%" (send gnucash num-accounts))
    (build-metadata gnucash)
    gnucash))

;; detect start of object definition and route to matching function
;; arg line: string current line (CR/LF and left-padding removed)
;; arg in: the input port
(define (dispatch-line gnucash-data line in)
  (cond
    [(equal? line ACCOUNT-START) (send gnucash-data add-account! (import-account in))]
    [(equal? line COMMODITY-START) (send gnucash-data add-commodity! (import-commodity in))]
    [(equal? line PRICE-START)(send gnucash-data add-price! (import-price in))]
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
  )


;; add matching transaction to account, account to split (not just id)
;; template transactions exit for accounts that were deleted - skip them
(define (link-split-account-transaction gnucash-data)
  (for ([transaction (send gnucash-data get-list-transactions)])
    (for ([split (send transaction get-splits)])
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
  (define (account-by-name arg-name)
      (let ([found-list (filter (lambda (act) (equal? arg-name (send act get-name))) (send gnucash-data all-accounts))])
        (if (empty? found-list)
            (error (format "can't find account with fullname ~a~%" arg-name))
            (car found-list))))
  (let ([accounts (send gnucash-data all-accounts)]
        [template-root-id (send (account-by-name %TEMPLATE-ROOT-FULLNAME%) get-id)])
    ; link to parent object
    (for ([act accounts])
      (let ([parent-id (send act get-parent-id)])
        (cond [(not (equal? "" parent-id))
             ; set-parent! also adds account as child to that parent
            (send act set-parent! (send gnucash-data account-by-id parent-id))])))
    ; remove templates; yes we loop again
    ; identify root
    ; build full name with recursive function
    (for ([act (send gnucash-data all-accounts)])
      (let ([account-name (send act get-name)]
            [account-id (send act get-id)] 
            [parent-id (send act get-parent-id)])
        ;(printf "name: ~a parent-id: ~a (t-r-id: ~a)~%" account-name parent-id template-root-id)
        
        (cond [(equal? account-name %ROOT-NAME%) (send gnucash-data set-root-account! act)]
              [(or (equal? account-name %TEMPLATE-ROOT-FULLNAME%) (equal? template-root-id parent-id))
              (send gnucash-data remove-account act)]
              [else (build-fullname act)])))

    ))

;; prepend the parent name to the fullname of the account
;; recurse up the parent tree
(define (build-fullname account)
  ; start by setting fullname to account name
  (send account set-fullname! (send account get-name))
  (let loop ([parent (send account get-parent)])
    (cond
         [(not (or (void? parent) (equal? %ROOT-NAME% (send parent get-name))))
               (let* ([fullname (send account get-fullname)]
                      [parent-name (send parent get-name)]
                      [new-fullname (string-append parent-name ":" fullname)])
                 (send account set-fullname! new-fullname)
                 (loop (send parent get-parent)))])))

  #|
  (if (void? current-parent)
      void
      (let* ([parent-name (send current-parent get-name)]
            [fullname (send account get-fullname)]
            [new-fullname (string-append parent-name ":" fullname)])
        (send account set-fullname! new-fullname)
        (build-fullname-rec account (send current-parent get-parent)))))
               
  |#

;; ----------
;;   DEMO
;; ----------


(define (display-all-trans-for-bmo-mastercard gnucash-data)
    (let ([account (send gnucash-data account-by-name "BMO Mastercard")])
      (displayln "ALL TRANSACTIONS IN 'BMO Mastercard'")
      (displayln "------------------------------------")
      (display-all-transactions-all-splits-in-account account)))


; return the price ON the exact date, or on the closest date LESS than the date
(define (price-on-closest-date gnucash-data commodity-id arg-date)
  (let ([price-list (send gnucash-data price-list-for-cmdty-id commodity-id)])
    (let loop ([prices price-list] [found-price (void)])
      (let* ([price (car prices)]
            [date (send price get-date)])
        (cond
          [(empty? price-list) (error (format "didn't find the price for ~a on ~a~%" commodity-id date))]
          [(equal? date arg-date) price]
          [(string<? date arg-date) (loop (rest prices) price)] ; closest price so far
          [(string>? date arg-date)
           (if (void? found-price)
               (error (format "didn't find the price for ~a on ~a~%" commodity-id date))
               found-price)])))))
          


(define (demo)
  (displayln "----------------------------")
  (displayln "         DEMO               ")
  (displayln "----------------------------")
  (define gnucash-data (import-gnucash-file HUGE-SAMPLE-GNUCASH-FILE))
  ;(define gnucash-data (import-gnucash-file SMALL-SAMPLE-GNUCASH-FILE))
  ;(print-overview gnucash-data)
  (displayln "----------------------------")
  ;(display-all-accounts gnucash-data )
  ;(define first-tran (first (send gnucash-data get-list-transactions)))
  ;(displayln first-tran)
  ;(displayln (send first-tran as-string-single-line))
  ;(let ([tran (first (send gnucash-data get-list-transactions))])
  ;  (displayln (send tran as-string)))
  ;(displayln (send  as-string))
  ;(display-all-transactions gnucash-data)
  ;(display-all-commodities gnucash-data)
  ;(display-all-trans-for-bmo-mastercard gnucash-data)
  #|
  (let* ([prices-hash (send gnucash-data get-prices-by-cmdty-id)]
         [price-lists (hash-values prices-hash)]
         [flat-price-list (flatten price-lists)])
    (printf "found ~a price-commodities~%" (hash-count prices-hash))    
    (printf "first individual prices ~a~%" (length flat-price-list))
    (for ([id (hash-keys prices-hash)])
      (printf "======== ~a =======~%" id)
      (for ([price (hash-ref prices-hash id)])
        (printf "~a~%" (send price as-string)))))
        ;(printf "~a~%" (send price as-string)))))
  ;(displayln (send (price-on-closest-date gnucash-data "VEQT" "2023-08-30") as-string))
  ;(displayln "")

  ;(filter (lambda (act) (send act is-holding?)) (send gnucash-data all-accounts))
  (for ([account (filter (lambda (act) (send act is-holding?)) (send gnucash-data all-accounts))])
    (displayln (send account get-fullname)))

  (let ([account (send gnucash-data account-by-fullname "3. Investissements:NON-ENREGISTRÉ")])
    (printf "type of ~a: ~a~%"  (send account get-name) (send account get-type))
    (printf "is ~a an investment account? ~a~%" (send account get-name) (send account is-investment?))
    (printf "is ~a a holding account? ~a~%" (send account get-name) (send account is-holding?)))
|#
  (for ([account (send gnucash-data holding-accounts)])
    (displayln (send account get-fullname)))
  
  

  )

(demo)

          
            
    
  


;; --------------
;;   UNIT TESTS
;; --------------

#|
(check-eq? (char-position "allo" #\l) 1)
(check-eq? (char-position "allo" #\x) -1)
(check-equal? (element-value "<name>bob<df") "bob")
(check-equal? (element-value "<name><df") "")
(check-equal? (element-value "name") "")
(check-equal? (element-value "name><df") "") ;; element does not start with <
(check-equal? (element-value "<name>asdfasdfa") "") ;; element doesn't end on same line: skip it
(check-equal? (element-value "asdfafda</name>") "") ;; element ends here, but we don't have the start, so skip it
|#