#lang racket

(require rackunit)

(provide format-currency)

(define (string-take-left string num-chars)
  (substring string 0 num-chars))

(check-equal? (string-take-left "1234" 2) "12")

(define (string-take-right string num-chars)
  (define len (string-length string))
  (substring string (- len num-chars)))

(check-equal? (string-take-right "1234" 2) "34")

(define (format-currency input)
  (if (string? input)
      (if (string-prefix? input "-")
          (string-append "-$" (substring input 1))
          (string-append "$" input))
      (let ([value (exact-round input)])
        ;(printf "value: ~a negative? ~a formatted: ~a~%</br>" value (negative? value) (format "-$~a" (abs value)))
        (if (negative? value)
            (format "-$~a" (add-commas (abs value)))
            (format "$~a" (add-commas value))))))

(define (add-commas integer)
  (cond
    [(not (exact? integer)) (error "add-commas only works with integers (exact numbers)")]
    [(< integer 1000) (~a integer)]
    [else
     (define chars (~a integer))
     (let loop ([chars chars] [result ""])
       (define num-chars (string-length chars))
       (cond [(< num-chars 4) (string-append chars result)]
             [ else
               (loop (string-take-left chars (- num-chars 3))
                    (string-append
                      ","
                      (string-take-right chars 3)
                      result))]))]))
                   
(check-equal? (add-commas 1) "1")
(check-equal? (add-commas 999) "999")
(check-equal? (add-commas 1001) "1,001")
(check-equal? (add-commas 999999) "999,999")
(check-equal? (add-commas 9999999) "9,999,999")
           
  
  
  