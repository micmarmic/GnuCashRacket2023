#lang racket
(require rackunit)

#|

TEST module for finance.rkt

We keep tests separate so keep code modules simpler and to
ensure tests are evaluated on demand instead of every time.

(Yes, there are pros and cons to this approach.)

|#

(require "gnucash-objects.rkt")

(require "finance.rkt")


;; NOW, write the tests :)

;;
;; TEST: price-on-closest-date
;;
;; (define (price-on-closest-date gnucash-data commodity-id arg-date)

;; test data
(define gnucash (make-object gnucash-data%))
(define commodity-id-1 "BMO")
(define commodity-id-2 "NRG")
(define prices
  (list
   (make-object price% commodity-id-1 "2020-12-31" 4.545)
   (make-object price% commodity-id-1 "2021-12-31" 3.59495)
   (make-object price% commodity-id-1 "2022-12-31" 6.4934)
   (make-object price% commodity-id-1 "2023-12-31" 8.8)))

;; does init price% work?
(for ([pr (in-list prices)])
  (send gnucash add-price! pr))
   
(let ([prices (send gnucash price-list-for-cmdty-id commodity-id-1)])
  (for ([pr prices])
    (displayln (send pr as-string))))
                
;; date is first in list: return that price
(check-equal?
 (send (price-on-closest-date gnucash commodity-id-1 "2020-12-31") get-value)
 4.545)
;; date is middle of list: return that price
(check-equal?
 (send (price-on-closest-date gnucash commodity-id-1 "2021-12-31") get-value)
 3.59495)
;; date is last in list: return that price
(check-equal?
 (send (price-on-closest-date gnucash commodity-id-1 "2023-12-31") get-value)
 8.8)
;; date is between two dates: take the price at older date
(check-equal?
 (send (price-on-closest-date gnucash commodity-id-1 "2022-09-25") get-value)
 3.59495)
;; date is beyond date of last price in list: latest price
(check-equal?
 (send (price-on-closest-date gnucash commodity-id-1 "2024-12-31") get-value)
 8.8)
;; exception: no prices for that commodity: WHAT DO WE DO????
(check-equal?
 (price-on-closest-date gnucash commodity-id-2 "2024-12-31")
 null)



