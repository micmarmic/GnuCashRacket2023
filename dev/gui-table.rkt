#lang racket

(require arguments)

(require racket/gui/easy
         racket/gui/easy/operator
         racket/string)

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
(define (number->string* n)
  (if n (number->string n) ""))

(define column-names '("Commodity" "Value" "ROI"))

(define sample-source-data
  (list
    (list "BMO" "2345" "12%")
    (list "BCE" "12486" "2%")))

(define sample-source-data2
  (list
    (list "BMO" 2000 "10%")
    (list "BCE" 14334 "3%")
    (list "APPL" 550 "-1%")))


(define nested-vector (vector (vector "Bob" 12) (vector "Tom" 55)))

(define (nested-vec-ref vec row col)
  (vector-ref (vector-ref vec row) col))

(define (nested-list-ref lst row col)
  (list-ref (list-ref lst row) col))

(nested-vec-ref nested-vector 0 1)

(define (nested-vec-set! vec row col val)
  (vector-set! (vector-ref vec row) col val))

(nested-vec-set! nested-vector 0 1 15)
(nested-vec-ref nested-vector 0 1)

(printf "Value from nested list: ~a~%" (nested-list-ref sample-source-data 0 1))

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
             (nested-vec-set! result source-row source-col (~a value))
             ))           
     result]))

       
(printf "Result: ~a~%" (nested-list->gui-string-vector sample-source-data))


(define @source (@ sample-source-data))
(define @entries (@source . ~> . (λ (source) (nested-list->gui-string-vector source))))

(define test-table
  (table column-names
      #:column-widths (list (list 0 100) (list 1 50) (list 2 100))
      @entries
    ))

(define @choice (@ "Set1"))
(define @choice-values (@ '("Set1" "Set2")))
(define @choices (@choice-values . ~> . (λ (value) value)))

(render
 (window
  #:size (list 400 600)
  (choice
   #:label "Selection:"
   #:selection @choice
   )
  (button
   "Change data"
   (λ ()
     (@source . := . sample-source-data2)))
   test-table))


#|
(define args
    (arguments (for/list ([_ (in-range 2)])
      (table
       '("Name" "Age" "Nationality")
       ;(vector (vector "Bo" "Bill") (vector "23" "12"))
       (nested-list->gui-string-vector sample-source-data)
       #:column-widths (list (list 0 100) (list 1 50) (list 2 100))
       ))
      ))

(define win-exp '(window
    #:size (list 400 600)
    (text "Allo")
    (text "Bob")
    ))


(define win
   (window
    #:size (list 400 600)
    (text "Allo")
    (text "Bob")
    ))
 
(define win2 (eval win-exp ns))

(define texts '("Text1" "Text2"))
(define (texts->string-exp texts)
  (foldr string-append ""
   (map (lambda (s) (format "(text \"~a\")\n" s)) texts)))
(define texts-exp (texts->string-exp texts))
(define win3-exp
  (string-append
   "(window #:size (list 400 600)\n"
      texts-exp
      test-table-source
   ")"))

(define win3-exp-for-eval (read (open-input-string win3-exp)))
  
win3-exp-for-eval
;(define win3-exp (

|#



