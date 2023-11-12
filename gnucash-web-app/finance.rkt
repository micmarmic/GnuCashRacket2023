#lang racket

(require racket/block)

(require "gnucash-objects.rkt")

(provide (all-defined-out))

#|

MODULE FINANCE

Function to perform financial calculation and provide numbers for views.

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

; return the price% ON the exact date, or on the closest date LESS than the date
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