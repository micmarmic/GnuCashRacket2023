#lang racket
(require rackunit)

(require "gnucash-parser.rkt"
         "gnucash-objects.rkt"
         "settings.rkt")

(define gnucash-file1 (import-gnucash-file TEST-GNUCASH-FILE-1))

(define account1 (send gnucash-file1
                       account-by-fullname
                       "Assets:Investments:Brokerage Account"))

(define account1-balances
  (list
   '("2022-04-25" 50000)
   '("2022-12-31" 49569.20)
   '("2023-02-02" 49138.40)
   '("2023-05-22" 36681.40)
   '("2023-05-30" 2826.40)
   '("2023-07-04" 14674.40)
   '("2023-12-02" 14874.40)                        
   ))


(for ([test-case (in-list account1-balances)])
  (define date (first test-case))
  (define expected (real->decimal-string (second test-case)))
  (define actual (real->decimal-string (account-balance-on-date account1 date)))
  (check-equal? actual expected))


(regexp-match #rx"<img.*>" "333<img caat>333")
