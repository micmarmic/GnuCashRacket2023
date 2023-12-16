#lang racket

(require racket/unit)

(define (string-take-left string num-chars)
  (substring 0 num-chars))

(check-equal (string-take-left "1234") "34")

(define (string-take-right string num-chars)
  (define len (string-length num-chars))
  (substring string (- len num-chars)))

(define (add-commas integer)
  (cond
    [(not (exact? integer)) (error "add-commas only works with integers (exact numbers)")]
    [(< integer 1000) integer]
    [else
     (define chars (~a integer))
     (let loop ([chars chars] [result ""])
       (define num-chars (string-length chars))
       (cond [(< num-chars 4) (string-append (list->string chars) result)]
             [ else
               (loop (string-take-left chars (- num-chars 3)
                                (string-append "," (list->string chars) result)))]))]))
                   
                 
           
  
  
  
    