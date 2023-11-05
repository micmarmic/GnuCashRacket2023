#lang racket
(require "gnucash-parser.rkt"
         "gnucash-objects.rkt")

(define %path-data-file% "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")
(define gnucash-data (import-gnucash-file %path-data-file%))
(print-overview gnucash-data)
(define account (send gnucash-data account-by-name "BMO Mastercard"))
(define transactions (send account transactions-sorted-by-date))

(printf "Account: ~a #Transactions: ~a~%" (send account get-name) (length transactions))


(struct ledger-line (date description account-name value total balance))

(define (list-ledger-lines->string arg-lines)
  (let loop ([lines arg-lines] [result ""])
    (if (null? lines)
        result
        (loop (rest lines)
              (string-append result (ledger-line->string (car lines)) "\n")))))

(define (ledger-line->string line)
  ;(format "dt:~a desc:~a act:~a val:~a tot:~a bal:~a"
  (format "~a ~a ~a ~a ~a ~a"
          (~a (ledger-line-date line) #:width 12)
          (~a (ledger-line-description line) #:width 30)
          (~a (ledger-line-account-name line) #:width 30)
          (~a (ledger-line-value line) #:width 8)
          (~a (ledger-line-total line) #:width 8)
          (~a (ledger-line-balance line) #:width 10)))


;; given splits, build list of ledge-line and return total for transation in current account
;; return a struct compiled-splits
(struct compiled-splits (display-values transaction-value))
(define (compile-splits arg-splits arg-account-id)
  (let loop-splits ([splits arg-splits] [display-values '()] [total 0])
    (if (null? splits)
        ; RETURN VALUE 
        (compiled-splits display-values total)
        (let* ([split (car splits)])
          (if (equal? (send split get-account-id) arg-account-id)
              ; outcome: don't add split for current account
              ; call loop with next split and current values
              (loop-splits (rest splits) display-values (send split get-value))
              ; outcome: add split to other account
              (let ([line
                     (ledger-line "" (send split get-memo)
                                  (send split get-account-name)
                                  (real->decimal-string (send split get-value))
                                  "" "")]
                    [new-total (if (equal? arg-account-id (send split get-account-id))
                                   (send split get-value)
                                   total)])
                (loop-splits (rest splits) (append  display-values (list line)) new-total)))))))
    
;; compile a list of ledger-lines for trans and splits in given account-id
(define (trans-in-account->ledger-lines gnucash-data arg-account-id)
  (let* ([account (send gnucash-data account-by-id arg-account-id)]
         [transactions (send account transactions-sorted-by-date)])           
  (let loop ([all-trans transactions] [list-lines '()] [balance 0])
    (if (null? all-trans)
        list-lines
        (let* ([trans (car all-trans)]
               [compiled (compile-splits (send trans get-splits) arg-account-id)]
               [total (compiled-splits-transaction-value compiled)]
               [new-balance (+ total balance)]               
               [trans-line
                 (ledger-line (send trans get-date-posted)
                              (send trans get-description)
                              "  "
                              "  "
                              (real->decimal-string total)
                              (real->decimal-string new-balance))])
          (loop (rest all-trans)
                (append list-lines (list trans-line)
                                         (compiled-splits-display-values compiled))
                new-balance))))))
                                        
      
(display (list-ledger-lines->string
          (trans-in-account->ledger-lines gnucash-data (send account get-id))))
