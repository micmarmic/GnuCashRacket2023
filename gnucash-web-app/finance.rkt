#lang racket

(require racket/block)

(require "gnucash-objects.rkt"
         "gnucash-parser.rkt"
         )

(provide (all-defined-out))

#|

MODULE FINANCE

==> call (roi-on-date) to get a list of account-roi for a web or text view
    text view can be printed with (print-list-account-roi)

Functions to perform financial calculation and provide numbers for views.

See end of this module for explantion of ACB after selling shares

|#



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

;; format value as real with given precision and with
(define (freal value precision width)  
  ; using ~a with real converter because ~r separates the - sign from the number
  (cond [(null? value) ""]
        [(list? value) "BUG! freal got list as value"]
        [else (~a (real->decimal-string value) #:min-width width #:align 'right)]))


(define (repeat-char->string char n)
  (build-string n (lambda (n) char)))

;; ------------
;;  ROI REPORT
;; ------------




(struct roi-line (commo-id shares price value cost gain-loss roi))

(struct account-roi (account total-roi-line child-roi-lines error-message))

(define (calc-grand-total-list-account-roi list-account-roi)
  (summarize-roi-lines
   (map (lambda (act-roi)
          (account-roi-total-roi-line act-roi))
        list-account-roi)))
    
  

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
    (printf "~a ~a ~a ~a ~a ~a ~a~%"
            (~a (roi-line-commo-id arg-line) #:min-width 14)
            shares-str
            price-str
            (freal (roi-line-value arg-line) 2 12)
            (freal (roi-line-cost arg-line) 2 12)
            (freal (roi-line-gain-loss arg-line) 2 12)
            ; sub1 from 12 width to account for %
            (string-append (freal (roi-line-roi arg-line) 2 11) "%"))))

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


; return a roi-line for the account on the given date
; return null if no shares
; combine the calculation of the shares and cost with the search for the price
; and the roi calculations
; notice: if there are no shares, there is no summary, and nothing is returned
; if there are no prices for a given commodity, set roi to null to signal an issue
(define (summary-roi-on-date gnucash-data account arg-date)
  (let* ([account-id (send account get-id)]
         [snapshot (snapshot-on-closest-date gnucash-data account-id arg-date)]      
         [cost (investment-snapshot-amount snapshot)]
         [shares (investment-snapshot-shares snapshot)]
         [commo-id (investment-snapshot-commodity-id snapshot)])         
    (if (zero? shares)
        null
        (let ([price-obj (price-on-closest-date gnucash-data commo-id arg-date)])
          (if (null? price-obj)
              ;; if there is no price, set roi to null to signal an issue
              (roi-line (format "~a: NO PRICE ON ~a" commo-id arg-date) shares 0 0 cost 0 0)
              (let* ([price (send price-obj get-value)]
                    [value (* shares price)]
                    [gain-loss (- value cost)]
                    [roi (if (zero? cost) 0 (* 100 (/ gain-loss cost)))])
                (roi-line commo-id shares price value cost gain-loss roi)))))))
        
        #|
        (printf "~a ~a ~a ~a ~a ~a ~a~%"
                (~a commo-id #:min-width 14)
                (freal shares 3 8)
                (freal price 3 12)
                (freal value 2 12)
                (freal cost 2 12)
                (freal gain-loss 2 12)
                ; sub1 from 12 width to account for %
                (string-append (freal performance 2 11) "%"))))))
|#
(define (summarize-roi-lines all-lines)
  (foldl (lambda (line result)
           (let* ([new-cost (+ (roi-line-cost line) (roi-line-cost result))]
                  [new-value (+ (roi-line-value line) (roi-line-value result))]
                  [gain-loss (- new-value new-cost)]
                  [roi (if (zero?  new-cost) 0 (* 100 (/ gain-loss new-cost)))])
             (roi-line "TOTAL" "" "" new-value new-cost gain-loss roi)))
         (roi-line "TOTAL" 0 0 0 0 0 0)
         all-lines))

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
                 (loop (rest splits)
                       ; expenses and amount for next iteration
                       new-shares
                       new-amount))]                       
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
  (let* ([account (send gnucash-data account-by-id arg-account-id)]
         [commodity-id (send account get-commodity-id)]
         [all-transactions (send account get-transactions)])
    (let loop ([transactions all-transactions]
               [main-snapshot (investment-snapshot commodity-id 0 0)])
      (if (empty? transactions)
          main-snapshot
          (block
           (let* ([trans (first transactions)]
                  [splits (send trans get-splits)]
                  ;; in this let*, call make-split-snapshot for the current transaction
                  ;; and update the total shares and amount accordingly
                  [snapshot (make-split-snapshot arg-date splits arg-account-id)]
                  [snapshot-shares (investment-snapshot-shares snapshot)]
                  [snapshot-amount (investment-snapshot-amount snapshot)]
                  [current-shares (investment-snapshot-shares main-snapshot)] ; in case shares sold
                  [current-amount (investment-snapshot-amount main-snapshot)]
                  [current-price-share (if (= 0 current-shares) 0 (/ current-amount current-shares))]
                  [new-shares (+ current-shares snapshot-shares)]
                  
                  ; special case 1: if you sell everything (0 shares) then cost goes to $0
                  ; (gains/losses are realized, but that is out of scope for the snapshot)

                  ; special case 2: if shares decreased, there was a sale of shares,
                  ; amount needs to be calculated accordingly
                  [new-amount
                   (cond [(= 0 new-shares) 0]
                         [(< new-shares current-shares)
                          (cost-after-sale current-amount current-shares snapshot-shares)]
                         [else (+ snapshot-amount current-amount)])]) 
             ; loop until past the desired date
             (if (string>? (send trans get-date) arg-date)
                 main-snapshot
                 ; TODO just main-snaphot?
                 ;(loop '() main-snapshot)
                 (loop (rest transactions) (investment-snapshot commodity-id new-shares new-amount)))))))))

;; --------------------------------------------------------------
;;  COMMODITY PRICE FOR COMMODITY AT DATE CLOSEST BUT NOT BEYOND
;; --------------------------------------------------------------

; return the price (value of a price%) ON the exact date, or on the closest date LESS than the date
; return null if there are no prices for this commodity
(define (price-on-closest-date gnucash-data commodity-id arg-date)
  (let ([price-list (send gnucash-data price-list-for-cmdty-id commodity-id)])
    (let loop ([prices price-list] [found-price null])
      (if (empty? prices)
        (if (null? found-price)
            (block
             (printf "price-on-closest-date: didn't find the price for ~a on ~a~%" commodity-id arg-date)
             null
             )
            found-price)
      ; else (list is not empty)
        (let* ([price (car prices)]
               [date (send price get-date)])
          (cond
            [(equal? date arg-date) price]
            [(string<? date arg-date) (loop (rest prices) price)] ; closest price so far
            [(string>? date arg-date)
             (if (null? found-price)
                 (block
                  (printf "price-on-closest-date: didn't find the price for ~a on ~a~%" commodity-id arg-date)
                  null)
                 found-price)]))))))

;; ------
;;  DEMO
;; ------

(define HUGE-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define demo-date "2022-12-31")

; return a list of account-roi
(define (roi-on-date gnucash-data arg-date)
  (let ([all-account-roi '()])
    (for ([account (send gnucash-data holding-accounts)])
      (let ([fullname (send account get-fullname)]
            [children (send account get-children)])
        ;; loop child accounts
        (let loop ([all-children children]
                   [all-lines '()]
                   [error-message ""])
          (if (empty? all-children)
              ;; all children processed: return
              (set! all-account-roi
                    (append all-account-roi
                            (list (account-roi
                                   account
                                   (summarize-roi-lines all-lines)
                                   all-lines
                                   error-message))))
              ;; process individual child
              ;; line will be '() if there is no info returned
              (let* ([line (summary-roi-on-date gnucash-data (first all-children) arg-date)])                     
                (printf "line: ~a" line)
                (when (null? line) "DEBUG got a void line back")
                (if (null? line)
                    ; just loop with next child
                    (loop (rest all-children) all-lines error-message)
                    ; add line, update error message if required
                    (let ([new-message
                     ;; if roi is zero, there was an error, assuming missing price
                          (if (not (zero? (roi-line-roi line)))
                               error-message                         ; 
                               (let* ([child-account-name (send (first all-children) get-name)]
                                      [new-message (format "No prices for ~a" child-account-name)])
                                 (if (equal? "" error-message)
                                     new-message
                                     (format "~a; ~a" error-message new-message))))])
                      (loop (rest all-children) (append all-lines (list line)) new-message))))))))
    all-account-roi))
                
              
          ;(let ([snapshot (snapshot-on-closest-date gnucash-data (send child get-id) demo-date)])
          ;  (when (> (investment-snapshot-shares snapshot) 0)
          ;    (displayln (investment-snapshot-as-string snapshot)))))))))
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