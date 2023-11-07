#lang racket

;; text version of pagination only displays page numbers, ...
;; current page *n; disabled button "(prev)"
;; current-page is 0-based
;; TODO - error checking
(define (pagination-text num-items items-per-page current-page)
  (let* ([num-pages (/ num-items items-per-page)]
         [previous (if (= current-page 1) "(Previous)" "Previous(-1)")]
         [next (if (= current-page num-pages) "(Next)" "Next(+1)")]
         [last-page (sub1 num-pages)])
    ;(printf "DEBUG num-items: ~a items-per-page: ~a num-pages: ~a current-page ~a~%" num-items items-per-page num-pages current-page)
    (for ([n (range 0 num-pages)])
      ;(printf "n: ~a~%" n)
      
      (cond [(= n 0) (printf " ~a" previous)]
            [(= n num-pages) (printf " ~a" next)]
            [(= n current-page) (printf " [~a]" current-page)]
            [else (printf " ~a" n)]))
    (displayln "")))

(for ([n (range 0 20)])
  (pagination-text 100 5 n))
 
  