#lang racket
(define (no-return bool)
   (when bool
     (displayln "bool is true")))

;(no-return #t)
(no-return #f)