#lang racket


;; ** TESTS IN finance-test.rkt **

; TODO remove racket/block
(require racket/block)


(require rackunit)

(require (file "gnucash-objects.rkt")
         (file "gnucash-parser.rkt")
         (file "allocation.rkt")
         )

(provide roi-on-date 
         calc-grand-total-list-account-roi
         
         account-roi-account
         account-roi-error-message
         account-roi-child-roi-lines
         account-roi-total-roi-line
         account-roi-cash
         account-roi-grand-total-value
         account-roi-grand-total-cost
         account-roi-ca
         account-roi-us
         account-roi-intl
         account-roi-fixed
         account-roi-other

         roi-line-commo-id
         roi-line-cost
         roi-line-value
         roi-line-gain-loss
         roi-line-roi
         roi-line-ca
         roi-line-us
         roi-line-intl
         roi-line-fixed
         roi-line-other
         print-roi-line
         make-roi-line-from-allocation-rec         

         price-on-closest-date
         summary-roi-on-date
         roi-line-equal
         roi-line
         summarize-roi-lines
         make-allocation-percent
         make-alloc-rec
         add-allocation-to-roi-line
         subtract-roi-line         
         ;;adjust-allocation-percent
         )

#|

MODULE FINANCE

==> call (roi-on-date) to get a list of account-roi for a web or text view
    text view can be printed with (print-list-account-roi)

Functions to perform financial calculation and provide numbers for views.

See end of this module for explantion of ACB after selling shares


HIERARCHY OF FUNCTIONS CALLS

Called from report view

1) roi-on-date

     summary-roi-on-date: account on date
       * summary-roi-on-date
         price-on-closest-date (cleaned and tested)
	 price-list-for-cmdty-id (from object, no tests)
	 roi-line-error-message (from struct, no tests)

   before returning account-roi
      summarize-roi-lines

2) calc-grand-total-list-account-roi

Called from roi-view.html

for struct roi-line
   roi-line-commo-id
   roi-line-cost
   roi-line-value
   roi-line-gain-loss
   roi-line-roi

from struct account-roi
   account-roi-total-roi-line


(define (summary-roi-on-date
(define (summarize-roi-lines
(define (investment-snapshot-as-string 
(define (snapshot-on-closest-date
(define (price-on-closest-date


|#


; TODO ROI should *NOT* include the value in cash, just the summary of the investments



;; ---------------------------------------------------------------
;;  FULL SNAPSHOT FOR ALL COMMODITIES IN HOLDING ACCOUNTS AT DATE
;; ---------------------------------------------------------------

;; DATA FOR REPORT IN THIS FORMAT
;;
;; ACCOUNT NAME
;;
;; COMMODITY - MARKET VALUE -     COST - GAIN/LOSS - ROI
;; CASH             1020.00
;; BMO              1000.00     900.00      100.00   11%
;; etc...

;; TODO NEED CASH AMOUNT FROM HOLDING ACCOUNT

;; ---------
;;  HELPERS
;; ---------

;; format value as real with given precision and width (number decimals)
(define (freal value precision width)  
  ; using ~a with real converter because ~r separates the - sign from the number
  (cond [(null? value) ""]
        [(list? value) "BUG! freal got list as value"]
        [else (~a (real->decimal-string value) #:min-width width #:align 'right)]))


(define (repeat-char->string char n)
  (build-string n (lambda (n) char)))

;; --------------------------------
;;  ROI DATA STRUCTURE AND HELPERS
;; --------------------------------

;; represents the numbers on a monthly statement PLUS the asset allocation of the value
(struct roi-line (commo-id shares price value cost gain-loss roi error-message
                           ca us intl fixed other) #:transparent)
; for testing - is there an easier way?
(define (roi-line-equal this-line that-line)
  (and (equal? (roi-line-commo-id this-line) (roi-line-commo-id this-line))
       (equal?(roi-line-shares this-line) (roi-line-shares this-line))
       (equal?(roi-line-price this-line) (roi-line-price this-line))
       (equal?(roi-line-value this-line) (roi-line-value this-line))
       (equal?(roi-line-cost this-line) (roi-line-cost this-line))
       (equal?(roi-line-gain-loss this-line) (roi-line-gain-loss this-line))
       (equal?(roi-line-roi this-line) (roi-line-roi this-line))
       (equal?(roi-line-error-message this-line) (roi-line-error-message this-line))))

(struct account-roi (account total-roi-line child-roi-lines error-message
                             cash grand-total-cost grand-total-value
                             ca us intl fixed other))

(define (calc-grand-total-list-account-roi list-account-roi)
  (if (or (void? list-account-roi)(empty? list-account-roi))
      (error "cannot calc grand total because list account roi is void or empty")
      (summarize-roi-lines
       (map (lambda (act-roi)
              (account-roi-total-roi-line act-roi))
            list-account-roi))))
    
  
; in case you need to print out an roi-line
(define (print-roi-line arg-line)
  (let* ([price (roi-line-price arg-line)]
        [price-str (if (or (and (string? price) (equal? price ""))
                           (= 0 price))
                       (~a "" #:min-width 12)
                       (freal price 3 12))]
        [shares (roi-line-shares arg-line)]
        [shares-str (if (or (and (string? shares) (equal? shares ""))
                           (= 0 shares))
                        (~a "" #:min-width 8)
                        (freal shares 3 8))])
    (printf "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%"
            (~a (roi-line-commo-id arg-line) #:min-width 14)
            shares-str
            price-str
            (freal (roi-line-value arg-line) 2 12)
            (freal (roi-line-cost arg-line) 2 12)
            (freal (roi-line-gain-loss arg-line) 2 12)
            ; sub1 from 12 width to account for %
            (string-append (freal (roi-line-roi arg-line) 2 11) "%")
            (roi-line-error-message arg-line)
            (roi-line-ca arg-line)
            (roi-line-us arg-line)
            (roi-line-intl arg-line)
            (roi-line-fixed arg-line)
            (roi-line-other arg-line)


            )))


; in case you need to print out an account-roi
(define (print-list-account-roi data)
  (for ([account-data data])
    (printf "~a~%" (repeat-char->string #\- 88))
    (printf "~a~%" (send (account-roi-account account-data) get-fullname))
    (printf "~a~%" (repeat-char->string #\- 88))
    (printf "~a ~a ~a ~a ~a ~a ~a~%"
            (~a "COMMODITY" #:min-width 14)
            (~a "SHARES" #:min-width 8 #:align 'right)
            (~a "PRICE" #:min-width 12 #:align 'right)
            (~a "MKT VALUE" #:min-width 12 #:align 'right)
            (~a "COST" #:min-width 12 #:align 'right)
            (~a "GAIN/LOSS" #:min-width 12 #:align 'right)
            (~a "ROI" #:min-width 12 #:align 'right))
    (for ([line (account-roi-child-roi-lines account-data)])
      (print-roi-line line))
    (print-roi-line (account-roi-total-roi-line account-data))))


;; subtract value and allocation fields only, leave other fields blank
(define (subtract-roi-line line1 line2)
  (let ([result 
         (roi-line
          "Difference" 0 0
          (- (roi-line-value line1) (roi-line-value line2))
          0 0 0 ""
          (- (roi-line-ca line1) (roi-line-ca line2))
          (- (roi-line-us line1) (roi-line-us line2))
          (- (roi-line-intl line1) (roi-line-intl line2))
          (- (roi-line-fixed line1) (roi-line-fixed line2))
          (- (roi-line-other line1) (roi-line-other line2)))])
    result))

;commo-id shares price value cost gain-loss roi error-message
;                           ca us intl fixed other) #:transparent)


;; ------------------------------
;;  ROI REPORT SUMMARY FUNCTIONS
;; ------------------------------

;; calc the alloc-rec for a roi-line for a given commodity
(define (make-alloc-rec commo-id value alloc-hash)
  (cond
    [(null? alloc-hash) (alloc-rec commo-id 0 0 0 0 0)]
    [else         
     (if (not (hash-has-key? alloc-hash commo-id))
         (error (format "There is no allocation data for commodity '~a'"))
         (let ([source-alloc-rec (hash-ref alloc-hash commo-id)])
           (alloc-rec
            commo-id
            (* value (alloc-rec-ca source-alloc-rec))
            (* value (alloc-rec-us source-alloc-rec))
            (* value (alloc-rec-intl source-alloc-rec))
            (* value (alloc-rec-fixed source-alloc-rec))
            (* value (alloc-rec-other source-alloc-rec)))))]))

; return a roi-line for the account on the given date
; return null if no shares
; combine the calculation of the shares and cost with the search for the price
; and the roi calculations
; allocation: also add the asset allocation at the target date if an alloc-hash is passed
; if there are no prices for a given commodity, set roi to 0 and add error message
(define (summary-roi-on-date gnucash-data account arg-date [alloc-hash null])
  (define account-id (send account get-id))
  (define snapshot (snapshot-on-closest-date gnucash-data account-id arg-date))
  (define cost (investment-snapshot-amount snapshot))
  (define shares (investment-snapshot-shares snapshot))
  (define commo-id (investment-snapshot-commodity-id snapshot))
  (cond
    [(zero? shares) null]
    [ else
      (define price-obj (price-on-closest-date gnucash-data commo-id arg-date))
      (cond
        [(null? price-obj)
         ; no price!!
         (roi-line commo-id shares 0 0 cost 0 0 (format "No price for ~a on ~a" commo-id arg-date)
                   ;; allocation: all zero
                   0 0 0 0 0)]
        [else   
         (define price (send price-obj get-value))
         (define value (* shares price))
         (define gain-loss (- value cost))
         (define roi (if (zero? cost) 0 (* 100 (/ gain-loss cost))))
         (define alloc-rec (make-alloc-rec commo-id value alloc-hash))
         (roi-line commo-id shares price value cost gain-loss roi ""
                   (alloc-rec-ca alloc-rec)
                   (alloc-rec-us alloc-rec)
                   (alloc-rec-intl alloc-rec)
                   (alloc-rec-fixed alloc-rec)
                   (alloc-rec-other alloc-rec)
                   )])]))


;; return a roi-line with the total for a given account
;; all-lines should all be from same account
(define (summarize-roi-lines all-lines)
  (foldl
   (lambda (line result)
     (define new-cost (+ (roi-line-cost line) (roi-line-cost result)))
     (define new-value (+ (roi-line-value line) (roi-line-value result)))
     (define gain-loss (- new-value new-cost))
     (define roi (if (zero?  new-cost) 0 (* 100 (/ gain-loss new-cost))))
     ;; TODO: allocation
     (roi-line "TOTAL" "" "" new-value new-cost gain-loss roi ""
               (+ (exact-round (roi-line-ca line)) (roi-line-ca result))
               (+ (exact-round (roi-line-us line)) (roi-line-us result))
               (+ (exact-round (roi-line-intl line)) (roi-line-intl result))
               (+ (exact-round (roi-line-fixed line)) (roi-line-fixed result))
               (+ (exact-round (roi-line-other line)) (roi-line-other result))))
   (roi-line "TOTAL" 0 0 0 0 0 0 "" 0 0 0 0 0 )
   all-lines))


;; change the values for the allocation for percents
;; input: roi-line
;; output: roi-line
(define (make-allocation-percent line)
  (if (zero? (roi-line-value line))
      line      
      (roi-line
       (roi-line-commo-id line)
       (roi-line-shares line)
       (roi-line-price line)
       (roi-line-value line)
       (roi-line-cost line)
       (roi-line-gain-loss line)
       (roi-line-roi line)
       (roi-line-error-message line)
       ;(format "~a%" (exact-round (* 100 (/ (roi-line-ca line) (roi-line-value line)))))
       (/ (roi-line-ca line) (roi-line-value line))
       (/ (roi-line-us line) (roi-line-value line))
       (/ (roi-line-intl line) (roi-line-value line))
       (/ (roi-line-fixed line) (roi-line-value line))
       (/ (roi-line-other line) (roi-line-value line))
       )))


(define (add-allocation-to-roi-line line target-alloc-rec)
  (if (zero? (roi-line-value line))
      line      
      (roi-line
       (roi-line-commo-id line)
       (roi-line-shares line)
       (roi-line-price line)
       (roi-line-value line)
       (roi-line-cost line)
       (roi-line-gain-loss line)
       (roi-line-roi line)
       (roi-line-error-message line)
       (exact-round (* (alloc-rec-ca target-alloc-rec) (roi-line-value line)))
       (exact-round (* (alloc-rec-us target-alloc-rec) (roi-line-value line)))
       (exact-round (* (alloc-rec-intl target-alloc-rec) (roi-line-value line)))
       (exact-round (* (alloc-rec-fixed target-alloc-rec) (roi-line-value line)))
       (exact-round (* (alloc-rec-other target-alloc-rec) (roi-line-value line)))
       )))


(define (make-roi-line-from-allocation-rec target-allocation)
  (roi-line "" 0 0 0 0 0 0 ""
   (alloc-rec-ca target-allocation)
   (alloc-rec-us target-allocation)
   (alloc-rec-intl target-allocation)
   (alloc-rec-fixed target-allocation)
   (alloc-rec-other target-allocation)))

;; ------------------------------------------------------------------
;;  RECALCULATE ALLOCATION BASED ON SELECTED COMMODITIES AND AMOUNTS
;; ------------------------------------------------------------------


;; given an allocation value (roi-line) return an roi-line, 
;; after recalculating based on the user input of commodity-id and value
;; and the allocation-hash defined the allocation per commodity
;; input-commodity-value hash is keyed on commodity ID and contains an amount (exact?)
;; example
;; current CA:
#|
(define (adjust-allocation-percent input-commodity-value-hash actual-allocation-percent-line allocation-hash)
  (let loop ([inputs (hash->list input-commodity-value-hash)])
    (cond [(empty? inputs) void]
          [else
           (define current-input (first inputs))
           (define commo-id (first current-input))
           (define value (second current-input))
           (loop (rest inputs))])))
|#
  
;; -----------------------------------------------
;;  SNAPSHOT AT DATE FOR SINGLE COMMODITY ACCOUNT
;; -----------------------------------------------

; local struct used to return multiple values from a function
(struct investment-snapshot (commodity-id shares amount))

;; format a snapshot for debugging
(define (investment-snapshot-as-string snapshot)
  (format "Commodity-id: ~a Shares: ~a Amount: ~a"
          (investment-snapshot-commodity-id  snapshot)
          (investment-snapshot-shares snapshot)
          (real->decimal-string (investment-snapshot-amount snapshot))))

;; calculate new cost after shares sold
(define (cost-after-sale current-cost current-shares share-diff)
  (let* ([share-diff (abs share-diff)]
        [cost-per-share (if (= 0 current-shares)
                            0
                            (/ current-cost current-shares))]
        [new-shares (- current-shares share-diff)]
        [new-cost (* cost-per-share new-shares)])
    new-cost))
        

;; find shares and amount for a list of splits (from a single transaction)
;; return a struct investment-snapshot
(define (make-split-snapshot arg-date arg-splits arg-account-id)  
  (let loop ([splits arg-splits] [shares 0] [amount 0])
    (if (empty? splits)
        ;; don't remember commodity-id at split level (see account level)
        ;(investment-snapshot "" shares amount)
        (investment-snapshot "" shares amount)         
        (let ([split (first splits)])
          (cond [(equal? (send split get-account-id) arg-account-id)
                 (let* ([split-shares (send split get-quantity)]
                        [new-shares (+ shares split-shares)]
                        [current-value (send split get-value)]
                        [new-amount (+ amount current-value)])
                 (loop (rest splits) new-shares  new-amount))]                       
                [(equal? TYPE-EXPENSE
                         (send (send split get-account) get-type))
                 (loop (rest splits)
                       ; amount changes for next iteration
                       (+ shares 0)
                       (+ amount  (send split get-value)))]
                [else
                 ; no change to shares, amount, just loop
                 (loop (rest splits) shares amount)])))))


;; --->>> CALL THIS FUNCTION
;; given an account and date, find the snapshot on closest-date not beyond
;; return an investment-snapshot
(define (snapshot-on-closest-date gnucash-data arg-account-id arg-date)
  (define account (send gnucash-data account-by-id arg-account-id))
  (define commodity-id (send account get-commodity-id))
  (define all-transactions (send account get-transactions))
  (let loop ([transactions all-transactions]
             [main-snapshot (investment-snapshot commodity-id 0 0)])
    (cond
      [(empty? transactions) main-snapshot]
      [else
       (define trans (first transactions))
       (cond
         [(string>? (send trans get-date) arg-date) main-snapshot]
         [else 
          (define splits (send trans get-splits))
          ;; in this let*, call make-split-snapshot for the current transaction
          ;; and update the total shares and amount accordingly
          (define snapshot (make-split-snapshot arg-date splits arg-account-id))
          (define snapshot-shares (investment-snapshot-shares snapshot))
          (define snapshot-amount (investment-snapshot-amount snapshot))
          (define current-shares (investment-snapshot-shares main-snapshot))
          (define current-amount (investment-snapshot-amount main-snapshot))
          (define current-price-share (if (= 0 current-shares) 0 (/ current-amount current-shares)))
          (define new-shares (+ current-shares snapshot-shares))
          
          ; special case 1: if you sell everything (0 shares) then cost goes to $0
          ; (gains/losses are realized, but that is out of scope for the snapshot)
          ; special case 2: if shares decreased, there was a sale of shares,
          ; amount needs to be calculated accordingly
          (define new-amount
            (cond [(= 0 new-shares) 0]
                  [(< new-shares current-shares)
                   (cost-after-sale current-amount current-shares snapshot-shares)]
                  [else (+ snapshot-amount current-amount)]))
          (loop (rest transactions) (investment-snapshot commodity-id new-shares new-amount))])])))
;; --------------------------------------------------------------
;;  COMMODITY PRICE FOR COMMODITY AT DATE CLOSEST BUT NOT BEYOND
;; --------------------------------------------------------------

; return the price% ON the exact date, or on the closest date LESS than the date
; return null if there are no prices for this commodity
(define (price-on-closest-date gnucash-data commodity-id arg-date)
  (define price-list (send gnucash-data price-list-for-cmdty-id commodity-id))
  (let loop ([prices price-list] [found-price null])
    (cond
      [(empty? prices)
       (if (null? found-price)
           null
           found-price)]
      [else 
       ; else (list is not empty)
       (define price (car prices))
       (define date (send price get-date))
       (cond
         [(equal? date arg-date) price]
         [(string<? date arg-date) (loop (rest prices) price)] ; closest price so far
         [(string>? date arg-date) found-price])])))

;; --------------------------
;;  ROI REPORT MAIN FUNCTION
;; --------------------------

(define HUGE-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define demo-date "2022-12-31")

; return a list of account-roi
(define (roi-on-date gnucash-data arg-date [alloc-hash null])
  (define all-account-roi '())
  (for ([account (send gnucash-data holding-accounts)])
    (define fullname (send account get-fullname))
    (define children (send account get-children))
    (define cash (account-balance-on-date account arg-date))
    ;; loop child accounts
    (let loop ([all-children children]
               [all-lines '()]
               [error-message ""])
      (if (empty? all-children)
          ;; all children processed: return
          (let* ([total-roi-line (summarize-roi-lines all-lines)]
                 [grand-total-cost (+ cash (roi-line-cost total-roi-line))]
                 [grand-total-value (+ cash (roi-line-value total-roi-line))]
                 [alloc-rec (make-alloc-rec "TARGET" grand-total-value alloc-hash)]                                        
                 [cash-text (real->decimal-string cash)]
                 [grand-total-cost-text (real->decimal-string grand-total-cost)]
                 [grand-total-value-text (real->decimal-string grand-total-value)]) 
            (set! all-account-roi
                  (append all-account-roi
                          (list (account-roi
                                 account
                                 total-roi-line
                                 all-lines
                                 error-message
                                 cash-text
                                 grand-total-cost-text
                                 grand-total-value-text                              
                                 (roi-line-ca total-roi-line)
                                 (roi-line-us total-roi-line)
                                 (roi-line-intl total-roi-line)
                                 (roi-line-fixed total-roi-line)
                                 (roi-line-other total-roi-line)
                                 )))))                               
          ;; process individual child
          ;; line will be '() if there is no info returned
          (let* ([line (summary-roi-on-date gnucash-data (first all-children) arg-date alloc-hash)])
            (if (null? line)
                ; just loop with next child
                (loop (rest all-children) all-lines error-message)
                ; add line, update error message if required, then loop
                (let* ([line-error (roi-line-error-message line)]
                       [new-message
                        (if (equal? "" line-error)
                            error-message                         ; 
                            (let* ([child-account-name (send (first all-children) get-name)])
                              (if (equal? "" error-message)
                                  line-error
                                  (format "~a; ~a" error-message line-error))))])                      
                  (loop (rest all-children) (append all-lines (list line)) new-message)))))))
  ; top-level, not in for!
  all-account-roi)

              
#|
(define gnucash-data (import-gnucash-file HUGE-SAMPLE-GNUCASH-FILE))
(define roi-report (roi-on-date gnucash-data demo-date))

(print-list-account-roi roi-report)
|#
#|
(define %path-data-file% "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define gnucash-data (import-gnucash-file %path-data-file%))
(define bmo-inv-id "13c0b98ed62cca3520c8f4bd500a9d63")
(define dates (list "2014-11-06" "2016-07-19" "2017-03-06"))

(define (snapshot-list-dates gnucash-data dates account-id)
  (let ([account (send gnucash-data account-by-id account-id)])
    (for ([date dates])
          (printf "~a: ~a~%" date(investment-snapshot-as-string (snapshot-on-closest-date gnucash-data account-id date))))))

(snapshot-list-dates gnucash-data dates bmo-inv-id)
|#

#|
Adjusting the ACB after you sell shares. 

When a sale transaction occurs, the new total ACB must be reduced from the previous total based on the number of shares that are sold. We will have a new total ACB and a new share count. 

New Total ACB After a Sell Transaction = [Previous Total ACB] – ([ACB per Share] x [Number of Shares Sold]) 

Here’s a simple example. 

    Purchase 100 shares at $75 – $10 commission – 100 share count. 
    Sell 50 shares at $100 – $10 commission – 50 share count. 

Original ACB is $7500 + $10 for a $760 total with a share count of 100. 

ACB per share is $75.10 

------>
Total ACB after sale. The capital gain does not affect the ACB per share, that is a separate tax event. 

$7510 x (100 shares – 50 shares)  / 100 shares) = Total ACB of $3755

ACB per share is still $75.10 but it is now based on 50 shares and that new Total ACB. 

The ACB after the purchase. 

50 shares at $125 + $10 = $6260 – new share count of 100. 

Total ACB is now $6260 + $3755 = $10,015 

ACB per share is now $10,015 / 100 = $100.15


SUMMARIZE

BUY 100 @ $75 - $10 comm = $760 => $75.10/shares
SELL 50 - remainder 50 @ $75.10 =

ALGORITHM
1) calc current price per share: cost / shares
2) reduce shares, cost is new share balance * original cost per share

|#
