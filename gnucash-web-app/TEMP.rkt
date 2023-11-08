#lang racket
(struct nav-link (href text li-class tab-index))
(define lnk1 (nav-link "ref" "txt" "cls" "idx"))
lnk1
(nav-link? lnk1)
(define lst (list lnk1))
(car lst)
(printf "~a~%" (car lst))
(printf "~a~%" (nav-link-href (car lst)))