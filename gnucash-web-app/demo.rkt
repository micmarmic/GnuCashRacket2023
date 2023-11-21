#lang racket
;; ----------
;;   DEMO
;; ----------

(require racket/block)

(require "gnucash-parser.rkt"
         "finance.rkt"
         "views.rkt"
         )

(define (demo)
  (displayln "----------------------------")
  (displayln "         DEMO               ")
  (displayln "----------------------------")
  (define gnucash-data (import-gnucash-file HUGE-SAMPLE-GNUCASH-FILE))
  (define account (send gnucash-data account-by-fullname "3. Investissements:REER:BCE (REER)"))
  (define account-id (send account get-id))
  (define arg-date "2023-10-31")
  (define commodity-id "BCE")
  
  (printf "------------------------------~%GET CASH VALUE FROM LEDGER~%")
  (define holding-account (send (send gnucash-data account-by-id (send account get-parent-id)) get-id))
  ; STEP 1. GET FULL LEDGER FOR THE **PARENT** ACCOUNT
  (define ledger-lines (bank-account->ledger-lines gnucash-data holding-account -1 #f 1000))
  ;(printf "Balance on ~a: ~a~%" arg-date (balance-of-ledger-on-closest-date gnucash-data ledger-lines arg-date))
  (balance-of-ledger-on-closest-date ledger-lines arg-date)

)

(define (balance-of-ledger-on-closest-date ledger-lines arg-date)
  #|
  (for/last ([n (in-range (length ledger-lines))]
                   #:when (string<=? (bank-ledger-line-date (car (list-ref ledger-lines n))) arg-date))
          (bank-ledger-line-balance (car (list-ref ledger-lines n)))))
|#
  (string->number
   (for/last ([line (in-list ledger-lines)]
                   #:when (string<=? (bank-ledger-line-date (car line)) arg-date))
          (bank-ledger-line-balance (car line)))))

 
(demo)

  