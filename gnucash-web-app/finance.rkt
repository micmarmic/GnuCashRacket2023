#lang racket

(require racket/block)

(require "gnucash-objects.rkt"
         "gnucash-parser.rkt"
         )

(provide (all-defined-out))

#|

MODULE FINANCE

Functions to perform financial calculation and provide numbers for views.

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

(define (freal value precision width)
  ; using ~a with real converter because ~r separates the - sign from the number
  (~a (real->decimal-string value) #:min-width width #:align 'right))

(define (repeat-char->string char n)
  (build-string n (lambda (n) char)))

; return a commodity-summary for the account on the given date
; combine the calculation of the shares and cost with the search for the price
; and the roi calculations
; notice: if there are no shares, there is no summary, and nothing is returned
(define (summary-roi-on-date gnucash-data account arg-date)
  (let* ([account-id (send account get-id)]
         [commodity-id (send account get-commodity-id)]
         [snapshot (snapshot-on-closest-date gnucash-data account-id arg-date)]      
         [cost (investment-snapshot-amount snapshot)]
         [shares (investment-snapshot-shares snapshot)]
         [commo-id (investment-snapshot-commodity-id snapshot)])         
    (when (> shares 0)
      (let* ([price (send (price-on-closest-date gnucash-data commodity-id arg-date) get-value)]
            [value (* shares price)]
            [gain-loss (- value cost)]
            [performance (* 100 (/ gain-loss cost))])
        (printf "~a ~a ~a ~a ~a ~a ~a~%"
                (~a commo-id #:min-width 14)
                (freal shares 3 8)
                (freal price 3 12)
                (freal value 2 12)
                (freal cost 2 12)
                (freal gain-loss 2 12)
                ; sub1 from 12 width to account for %
                (string-append (freal performance 2 11) "%"))))))


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
                 (let* ([new-shares (+ shares (send split get-quantity))]
                       [new-amount (+ amount (send split get-value))])
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
                  [new-shares (+ (investment-snapshot-shares main-snapshot) (investment-snapshot-shares snapshot))]
                  ; special case: if you sell everything (0 shares) then cost goes to $0
                  ; (gains/losses are realized, but that is out of scope for the snapshot)
                  [new-amount (if (= 0 new-shares)
                                  0
                                  (+ (investment-snapshot-amount main-snapshot) (investment-snapshot-amount snapshot)))])
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
               (send found-price get-value))])))))

;; ------
;;  DEMO
;; ------

(define HUGE-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define demo-date "2022-12-31")

(define (demo arg-date)
  (displayln "DEMO IN FINANCE.RKT")
  (let ([gnucash-data (import-gnucash-file HUGE-SAMPLE-GNUCASH-FILE)])
    (printf "DATE: ~a~%" arg-date)
    (for ([account (send gnucash-data holding-accounts)])
      (let ([fullname (send account get-fullname)]
            [children (send account get-children)])
        (printf "~a~%" (repeat-char->string #\- 88))
        (printf "~a~%"  fullname)
        (printf "~a~%" (repeat-char->string #\- 88))
        (printf "~a ~a ~a ~a ~a ~a ~a~%"
                (~a "COMMODITY" #:min-width 14)
                (~a "SHARES" #:min-width 8 #:align 'right)
                (~a "PRICE" #:min-width 12 #:align 'right)
                (~a "MKT VALUE" #:min-width 12 #:align 'right)
                (~a "COST" #:min-width 12 #:align 'right)
                (~a "GAIN/LOSS" #:min-width 12 #:align 'right)
                (~a "ROI" #:min-width 12 #:align 'right))
        (for ([child children])
          ;(printf "~a~%"  (send child get-fullname))
          (summary-roi-on-date gnucash-data child arg-date))))))
          ;(let ([snapshot (snapshot-on-closest-date gnucash-data (send child get-id) demo-date)])
          ;  (when (> (investment-snapshot-shares snapshot) 0)
          ;    (displayln (investment-snapshot-as-string snapshot)))))))))


(demo demo-date)