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
              (string-append result "\n   " (ledger-line->string (car lines)))))))

(define (ledger-line->string line)
  (format "dt:~a desc:~a act:~a val:~a tot:~a bal:~a~%"
          (ledger-line-date line)
           (ledger-line-description line)
           (ledger-line-account-name line)
           (ledger-line-value line)
           (ledger-line-total line)
           (ledger-line-balance line)))
           

; splits formatted for display and transaction value for current account
(define (compile-all-trans transactions arg-account-id)
  (struct compiled-splits (display-values transaction-value))
  (for ([trans transactions])
    ;(printf "~a~%" (send trans get-date-posted))
    (let ([compiled 
           (let loop-splits ([splits (send trans get-splits)] [display-values '()] [total 0])
             (if (null? splits)
                 (compiled-splits display-values total)
                 (let* ([split (car splits)]
                        [line
                         (ledger-line "" (send split get-memo)
                                      (send split get-account-name)
                                      (real->decimal-string (send split get-value))
                                      "" "")]
                        [new-total (if (equal? arg-account-id (send split get-account-id))
                                   (send split get-value)
                                   total)])
                   (loop-splits (rest splits) (append  display-values (list line)) new-total))))])
             (printf "Tot:~a Vals:~a~%" (real->decimal-string (compiled-splits-transaction-value compiled))
                                         (list-ledger-lines->string (compiled-splits-display-values compiled))))))

(compile-all-trans transactions (send account get-id))
