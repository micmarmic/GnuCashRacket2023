#lang racket
(require rackunit)

(define ns (make-base-namespace)) ;; needed for eval to work

(define (test)
  (let ([value
         (cond [(equal? 1 2) 1]
               [(equal? 2 2) 2])])
    value))
  
(test)



(define (content->string content)
  (flatten-content content ""))

(define (flatten-content content result)
  (cond [(string? content) (string-append result content)]
        [(null? content) result]        
        [(let* ([head (first content)]
               [tail (rest content)]
               [value (if (string? head) head (eval head ns))])           
           (flatten-content tail (string-append result value)))]))


;(flatten-content test-mixed-list)

(check-equal? (content->string '("a")) "a")
(check-equal? (content->string '("a" "B")) "aB")
(define test-mixed-list  '("a" (format "~a" (+ 1 2)) "B"))
(check-equal? (content->string test-mixed-list) "a3B")


(define (element name class-values content #:new-line new-line)
  (let ([line-breaks (cond [(equal? new-line 'before-after) (list "\n" "\n")]
                           [(equal? new-line 'before) (list "\n" "")]
                           [(equal? new-line 'after) (list "" "\n")]
                           [else '("" "")])]
        [class-code (if (equal? class-values "") "" (format " class=\"~a\"" class-values))])
    (format "<~a~a>~a~a</~a>~a" name class-code (first line-breaks)  (content->string content) name (second line-breaks))))

(define p
  (lambda content
    (element "p" "" content #:new-line 'after)))

(define b
  (lambda content
    (element "b" "" content  #:new-line 'none)))

(define div
  (lambda content
    (element "div" "" content  #:new-line 'before-after)))

(define p-c
  (lambda class-content
    (let ([class-values (first class-content)]
          [rest-code (content->string (rest class-content))])
    (format "<p class=\"~a\">~a</p>~%" class-values rest-code))))


(define div-c
  (lambda class-content
    (let ([class-values (first class-content)]
          [rest-code (content->string (rest class-content))])
      (printf "rest code ~a~%" rest-code)
      (format "<div class=\"~a\">~%~a</div>~%" class-values rest-code))))
    
    ;(element-c (cons "div" class-content))))
  
;(displayln (p "hello"))
(displayln (p "Hello " (b "World") "!"))
(displayln (p-c "a-class;another;bye" "Hello " (b "World") "!"))
(displayln (div (p "Hello " (b "World") "c")))
(displayln (div-c "important"
                  (p "Hello " (b "World") "!")
                  (p "I am " (b "here") ".")))



#|
(define element
  (lambda (name class-name content)
    (printf "~a~%" content)
    (let ([joined-content (if (list? content) (apply (string-append) (map eval content)) content)]
          [class (if (equal? "" class-name) "" (format-class class-name))])
      (format "<~a~a>~a</~a>~%" name class joined-content name))))
  
(define b
  (lambda content
    (element "b" "" content)))

(define p
  (lambda content
    (element "p" "" content)))

(define div
  (lambda (class-name content)
    (element "div" "" content)))


(define (format-class class-name)
    (if (equal? class-name "")
        ""
        (format " class=\"~a\"" class-name)))


(define (string-append-list lst)
  (if (null? lst)
      ""      
      (string-append (car lst) (string-append-list (cdr lst)))
      ))


;; use an optional argument

(define p
  (lambda content
    (let ([final-content (if (list? content) (string-append-list content) content)]
          [class ""])
      (format "<p~a>~a</p>~%" (format-class class) final-content))))

(define p
  (lambda rest)    
    (format "<p~a>~a</p>~%" (format-class class)content))

(define div
  (lambda (content [class ""])
    (format "<div~a>~%~a</div>~%" (format-class class) content)))


;; use a rest argument

(define div
  (lambda content-list [class-opt ""])
    (let ([joined-contents (string-join content-list "")]
          [[class (if (equal? "" class-opt) "" (format " class=\"~a\"" class-opt))])
      (format "<div~a></div>"


;(display (p "Hello"))
(displayln "=======================")
;(display (b "bold"))
(displayln "=======================")
;;(display (p "Hello " (b "World")))
(displayln "=======================")
;(display (div "important" (p "hello")))
(displayln "=======================")
;(display (div "important" "hello"))
(displayln "=======================")

(displayln
  (div ""
   (div ""
    (div ""
     (p "hello")))))


;; TEST QUOTE UNQUOTE
;(b "hello" #:class-name "")



|#