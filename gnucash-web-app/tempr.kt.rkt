#lang racket
(require rackunit)

;; take n items starting at index x (0-based)
(define (take-n-from-x lst n x)
  (take (drop lst (sub1 x)) n))
(check-equal? (take-n-from-x '(1 2 3 4 5 6 7 8 9) 5 3)
              '(3 4 5 6 7))
(check-equal? (take-n-from-x '(1 2 3 4 5 6 7 8 9) 3 5)
              '(5 6 7))
;; given a list of page numbers (for pagination)
;; reduce list to no more than max-page-numbers
;; examples:

;; 
;; P is last page number
;; p is current-page number
;; p <= 3
;; => first 5 ... last
;; [1] 2 3 4 5 ...
;; 1 [2] 3 4 5 ...
;; 1 2 [3] 4 5 ...
;; 1 2 3 [4] 5 6 ...
;; 5 <= p <= P - 4
;; 1 ... 3 4 [5] 6 7 ...
;; 1 ... 4 5 [6] 7 8 ...
;; 17 18 [19] 20 21 .. 23
;; ---------
;; p >= P - 3
;; => 1 ... last 5
;; 19 [20] 21 22 23
;; 19 20 [21] 22 23
;; 19 20 21 [22] 23
;; 19 20 21 22 [23]

;; 1 ... 5 6 [7] 8 9 ...
;; reciprocal at the end
;; len-chunk is the number of links in the block (... 5 6 7 8 ... is 4)
(define (truncated-pagination numbers current-page len-chunk)
  (let ([num-pages (- (length numbers) 2)]) ; len number - 2 for prev and next
    (if (< (length numbers) %max-page-numbers%)
        numbers
        ;      end of list
        (cond [(> current-page (- num-pages 3)) ; P - 3 (-1 for next)
               ; take 6 for 5 pages + next
               (flatten (list (take numbers 2) "..." (take-right numbers 6)))]
              ; border case at the start
              [(= current-page (sub1 len-chunk)) 
               (flatten (list (take numbers (+ 2 len-chunk)) "..." (take-right numbers 2)))]
              ; border case at the end
              [(= current-page (- num-pages 3)) 
               (flatten (list (take numbers 2) "..."
                              (take-right numbers (+ 2 len-chunk))))]
              ; start of list
              [(< current-page (sub1 len-chunk)) 
               (flatten (list (take numbers (add1 len-chunk)) "..." (take-right numbers 2)))]
              ; middle
              [else              
               (flatten (list (take numbers 2)
                              "..."  
                              (take-n-from-x numbers len-chunk (sub1 current-page))
                              "..."
                              (take-right numbers 2)))]))))
#|
      (let ([last-two (take-right numbers 2)])
        (cond  
        (list "1" "2" "..." (~a (first last-two)) (~a (second last-two)))
        )))
|#

(define (print-page-numbers numbers)
  (for ([n numbers])
    (printf "~a " n))
  (displayln ""))

;;
;; TEST WITH CONTEXT
;;

(define %len-chunk% 5)
(define %max-page-numbers% 12)
(define %list-page-num-str% (flatten (list "prev" (map (lambda (n) (~a n)) (range 1 101)) "next")))
(define %links-to-display% 5)
(displayln "page 1")
(check-equal? (truncated-pagination %list-page-num-str% 1 %len-chunk%)
              '("prev" "1" "2" "3" "4" "5" "..." "100" "next"))
(displayln "page 2")
(check-equal? (truncated-pagination %list-page-num-str% 2 %len-chunk%)
              '("prev" "1" "2" "3" "4" "5" "..." "100" "next"))
(displayln "page 3")
(check-equal? (truncated-pagination %list-page-num-str% 3 %len-chunk%)
              '("prev" "1" "2" "3" "4" "5" "..." "100" "next"))
; TODO
(displayln "page 4")
(check-equal? (truncated-pagination %list-page-num-str% 4 %len-chunk%)
              '("prev" "1" "2" "3" "4" "5" "6" "..." "100" "next"))

(displayln "page 5")
(check-equal? (truncated-pagination %list-page-num-str% 5 %len-chunk%)
              '("prev" "1" "..." "3" "4" "5" "6" "7" "..." "100" "next"))

(displayln "page 6")
(check-equal? (truncated-pagination %list-page-num-str% 6 %len-chunk%)
              '("prev" "1" "..." "4" "5" "6" "7" "8" "..." "100" "next"))

(displayln "page 100")
(check-equal? (truncated-pagination %list-page-num-str% 100 %len-chunk%)
              '("prev" "1" "..." "96" "97" "98" "99" "100" "next"))
(displayln "page 99")
(check-equal? (truncated-pagination %list-page-num-str% 99 %len-chunk%)
              '("prev" "1" "..." "96" "97" "98" "99" "100" "next"))
(displayln "page 98")
(check-equal? (truncated-pagination %list-page-num-str% 98 %len-chunk%)
              '("prev" "1" "..." "96" "97" "98" "99" "100" "next"))
(displayln "page 97")
(check-equal? (truncated-pagination %list-page-num-str% 97 %len-chunk%)
              '("prev" "1" "..." "95" "96" "97" "98" "99" "100" "next"))
(displayln "page 96")
(check-equal? (truncated-pagination %list-page-num-str% 96 %len-chunk%)
              '("prev" "1" "..." "94" "95" "96" "97" "98" "..." "100" "next"))

;(print-page-numbers (truncated-pagination %list-page-num-str% current-page number-pages))
