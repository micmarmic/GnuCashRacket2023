#lang racket

#|

TEST module for gnucash-objects.rkt

We keep tests separate so keep code modules simpler and to
ensure tests are evaluated on demand instead of every time.

(Yes, there are pros and cons to this approach.)

|#


(require "gnucash-objects.rkt"
         "gnucash-parser.rkt"
         "settings.rkt"
         rackunit)


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


; convert numbers to string to avoid issued with 13 3/4 !- 13.75
(for ([test-case (in-list account1-balances)])
  (define date (first test-case))
  (define expected-balance (real->decimal-string (second test-case)))
  (define actual-balance (real->decimal-string (account-balance-on-date account1 date)))
  (printf "~a -> Actual: ~a Expected: ~a~%" date actual-balance expected-balance) 
  (check-equal? actual-balance expected-balance))
