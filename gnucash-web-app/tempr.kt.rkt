#lang racket
(define (multiconditions)
  (let ([num 100])
    (cond [(> num 2) displayln "matched 1"]
          [(> num 4) displayln "matched 2"])))

(multiconditions)