#lang racket

(require arguments)

(require racket/gui/easy
         racket/gui/easy/operator
         racket/string)

(require (file "../views/gui-views.rkt"))

(provide main-gui)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

#|
(define @csv (@ "a,b"))
(define @entries (@csv . ~> . (λ (csv)
                                (for/vector ([row (in-list (string-split csv "\n"))])
                                  (for/vector ([col (in-list (string-split row ","))])
                                    col)))))


(define @selection (@ #f))
|#

(define (right-align-fixed-text width text)
  (~a text #:min-width width #:left-pad-string " " #:align 'right))



(define (number->string* n)
  (if n (number->string n) ""))

(define column-names
  (list
    "Commodity"
    (right-align-fixed-text 10 "Cost")
    (right-align-fixed-text 10 "Value")
    (right-align-fixed-text 10 "Gain/Loss")
    (right-align-fixed-text 10  "ROI")))

#|
(define sample-source-data
  (list
    (list "BMO" "2345" "12%")
    (list "BCE" "12486" "2%")))

(define sample-source-data2
  (list
    (list "BMO" 2000 "10%")
    (list "BCE" 14334 "3%")
    (list "APPL" 550 "-1%")))
|#

(define nested-vector (vector (vector "Bob" 12) (vector "Tom" 55)))

(define (nested-vec-ref vec row col)
  (vector-ref (vector-ref vec row) col))

(define (nested-list-ref lst row col)
  (list-ref (list-ref lst row) col))

;(nested-vec-ref nested-vector 0 1)

(define (nested-vec-set! vec row col val)
  (vector-set! (vector-ref vec row) col val))

;(nested-vec-set! nested-vector 0 1 15)
;(nested-vec-ref nested-vector 0 1)

;(printf "Value from nested list: ~a~%" (nested-list-ref sample-source-data 0 1))

(define (make-nested-vector rows cols [init-val null])
  ; create empty vector of n rows
  (define result (make-vector rows init-val))
  ; fill each row with a vector of n cols (a new vector for each row!)
  (for ([row-num (in-range rows)])
    (vector-set! result row-num (make-vector cols init-val)))
  result)


(define (nested-list->gui-string-vector nested-list)
  (cond
    [(null? nested-list) (error "List is empty")]
    [else
     (define num-source-cols (length (first nested-list)))
     (define num-source-rows (length nested-list))
     (define result (make-nested-vector num-source-rows num-source-cols))
     (for ([source-row (in-range num-source-rows)])
           (for ([source-col (in-range num-source-cols)])
             (define value (nested-list-ref nested-list source-row source-col))
             ; remember: value to string
             (if (zero? source-col)
                 (nested-vec-set! result source-row source-col (~a value))             
                 (nested-vec-set! result source-row source-col
                                  (right-align-fixed-text 10 value)))
             ))
     result]))

#|
(define @source (@ sample-source-data))
(define @entries (@source . ~> . (λ (source) (nested-list->gui-string-vector source))))


(define test-table
  (table column-names
      #:column-widths (list (list 0 100) (list 1 50) (list 2 100))
      @entries
    ))
|#
;(printf "Result: ~a~%" (nested-list->gui-string-vector sample-source-data))


;; (define @source (@ sample-source-data))

(define @choice (@ "Set1"))
(define @choice-values (@ '("Set1" "Set2")))
(define @choices (@choice-values . ~> . (λ (value) value)))

;; rotate through data in hash
;; input: hash, hashkeys, current index in hashkeys
;; call with index = -1 to get first account
(define (next-account-and-index data-hash current-index)
  (define keys (hash-keys data-hash))
  (cond [(or (= -1 current-index)
             (= (add1 current-index) (length keys)))
         (values (hash-ref data-hash (list-ref keys 0)) 0)]
        [else
         (let ([next-index (add1 current-index)])
           (values (hash-ref data-hash (list-ref keys next-index)) next-index))]))
  


(define (main-gui gnucash-data allocation-data)
  (define accounts-hash (get-roi-table-data gnucash-data "2023-12-31"))
  (define grand-total-data (hash-ref accounts-hash "GRAND TOTAL"))
  (hash-remove! accounts-hash "GRAND TOTAL")
  (printf "hash-keys ~a~%" (hash-keys accounts-hash))
  (define keys (hash-keys accounts-hash))
  (printf "hash-keys ~a~%" (hash-keys accounts-hash))
  (define num-keys (length keys))
  (define-values (first-account-data index) (next-account-and-index accounts-hash -1))
  (displayln accounts-hash)

  (define @grand-total-source (@ grand-total-data))
  (define @grand-total-entries 
    (@grand-total-source . ~> . (λ (@grand-total-source)
                                  (nested-list->gui-string-vector @grand-total-source))))
  
  
  (define @account-name (@ (list-ref keys index)))
  (define @source (@ first-account-data))
  (define @entries (@source . ~> . (λ (source) (nested-list->gui-string-vector source))))

  (render
   (window
    #:title "GnuCash ROI Report"
    #:size (list 800 400)
    (text @account-name #:font (font "System" 11))
    (table column-names
           #:column-widths (list (list 0 200) (list 1 100) (list 2 100) (list 3 100) (list 4 100))
           #:stretch '(#t #t)
           #:font (font "Consolas" 11 #:family 'modern)
           @entries
           )
    (button
     "Next Account"
     (λ ()
       (begin
         (define-values (new-data next-index) (next-account-and-index accounts-hash index))
         (set! index next-index)
         (@account-name . := . (list-ref keys index))
         (@source . := . new-data))))
    (text "GRAND TOTAL" #:font (font "System" 11))
    (table column-names
           #:column-widths (list (list 0 200) (list 1 100) (list 2 100) (list 3 100) (list 4 100))
           #:stretch '(#t #f)
           #:font (font "Consolas" 11 #:family 'modern)
           @grand-total-entries
           )
    
    )))

;(main-gui)