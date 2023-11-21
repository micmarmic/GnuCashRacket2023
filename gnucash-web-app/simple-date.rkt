#lang racket
(require racket/date)
(require rackunit)

(provide end-last-month)

; copied somewhere :)
(define (leap-year? year)
  (or (and (zero? (remainder year 4))
           (not (zero? (remainder year 100))))
      (zero? (remainder year 400))))

; add 0 at start of list so month = index, eg. jan = 1
(define days-in-month-db '(0 31 28 31 30 31 30 31 31 30 31 30 31))

(define (days-in-month year month)
  (if (= 2 month)
      (if (leap-year? year) 29 28)
      (list-ref days-in-month-db month)))

(check-equal? (days-in-month 2023 2) 28)
(check-equal? (days-in-month 2024 2) 29)


; return string "yyyy-mm-dd"
(define (end-last-month)
  (str-date-end-last-month (current-date)))

; input is struct-date
(define (str-date-end-last-month struct-date)
  (let* ([day (date-day struct-date)]
        [month (date-month struct-date)]
        [year (date-year struct-date)]
        [last-month (if (= month 1) 12 (- month 1))]
        [adjust-year (if (= month 12) (- year 1) year)]
        [last-day (days-in-month adjust-year last-month)]
        [end-last-month (format "~a-~a-~a" adjust-year last-month last-day)])
    end-last-month))
            

;(define test-date (date 0 0 0 14 3 2024 0 0 #f 0))
;(displayln (str-date-end-last-month test-date))
;(printf "start last month: ~a~%" (str-date-end-last-month (current-date)))
  
