#lang racket


(require "gnucash-objects.rkt")

(define p1 (make-vanilla-price))

(send p1 as-string)

(define p2 (make-object price% "MBO" "2022-04-12" 34.556))
(send p2 as-string)


(define func-with-opt
  (lambda 