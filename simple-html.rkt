#lang racket
(require rackunit)
(require racket/block)
(define ns (make-base-namespace)) ;; needed for eval to work (flatten-content)

#|
TODO: MODULE HEADER


|#




(define INDENT "   ")

;; Return a string containing an HTML unordered list based on the class and data-rows.
;; This is a simple library: the class is applied at the <ul> level,
;; use <span> in the data-rows elements to apply additional formatting.
;; DEPENDS on INDENT to add a minimum of code indentation in the HTML output.
(define (ulist css-class data-rows)
  (let* ([class-code (if (equal? css-class "") "" (format " class=\"~a\"" css-class))]
         [ul-open-line (format "~a<ul~a>~%" INDENT class-code)]
         [ul-close-line (format "~a</ul>~%" INDENT)])
   (let loop ([data data-rows]
              [result ul-open-line])
     (cond [(null? data) (string-append result ul-close-line)]
           [else (loop
                  (rest data)
                  (string-append result (format "~a~a<li>~a</li>~%" INDENT INDENT (first data))))]))))
     
;; -----------------
;; SAVE HTML TO FILE
;; -----------------

;; save the text to the given path; replace the file if it exists
(define (save-to-file-with-replace text path)
  (call-with-output-file path #:exists 'replace
  (lambda (out)
    (fprintf out text))))

;; DEMO
;(define sample-html-file-path (build-path (current-directory) "HTML_EXAMPLE.html"))
;(save-to-file-with-replace (ulist "list-style" '(1 2 3 4)) sample-html-file-path)


;; convert a list of strings and expressions to a string
;; use eval to execute nested forms
;; DEPENDS on "(define ns (make-base-namespace))" before this function
(define (content->string content)
  (flatten-content content ""))

;; recursive function companion for content->string
;; recurse list of elements in the "content" form, returning strings
;; and executing nested function calls.
;; ASSUMES values are strings and nested forms produce strings
;; TODO? force convert value to string? or let it fail?
(define (flatten-content content result)
  (cond [(null? content) result]
        [(string? content) (string-append result content)]                
        [(let* ([head (first content)]
               [tail (rest content)]
               ;; eval here
               (print head)
               [value (if (string? head) head (eval head ns))])           
           (flatten-content tail (string-append result value)))]))


;; generic function to generate HTML code as strings
;; see all the other functions that use it (p, div, etc.)
;; name: the name of the HTML element like p, ul, h1, etc.
;; class-values: the string to insert in the class attribute,
;;               like "blog-post error" - this functions just inserts it in class="HERE"
;; content: form with stuff to insert in element; strings of forms that evaluate to string
;; #:new-line: add a newline 'before, 'none, 'before-after the element to make the output readable
(define (element name class-values content #:new-line new-line)
  (let ([line-breaks (cond [(equal? new-line 'before-after) (list "\n" "\n")]
                           [(equal? new-line 'before) (list "\n" "")]
                           [(equal? new-line 'after) (list "" (string-append "\n"))]
                           [else '("" "")])]
        [class-code (if (equal? class-values "") "" (format " class=\"~a\"" class-values))])
    (format "~a<~a~a>~a~a</~a>~a"
            INDENT
            name
            class-code
            (first line-breaks)
            (content->string content) name (second line-breaks))))

(define (class-code str)
  (if (equal? "" str) "" (format " class=\"~a\"" str)))

;; if the last element in the list contains is (class-code str), return the formatted class code
;; else return ""
;; format is " class=\"class code passed in class-code form\"
;; the caller should can check the return and ignore that last element i
;; to pass a class to the element, include a (gen-call "whatever") in the content
(define (element2 name content #:new-line new-line)
  ;(displayln (string-append "in element2 - name: " name " content:" (~a content)))
  (let* ([len (length content)]
         [penult (if (> len 2) (list-ref content (- len 2)) "")])    
         (if (equal? 'class penult)
             (block
              ;(printf "pen: ~a~%" penult)
              (let* ([values (apply string-append (drop-right content 2))]
                     [class-names (last content)])
                (element name class-names values #:new-line new-line)))
             (block
              ;(displayln (string-append "in element2 - no class:" (~a content) " penult: " (~a penult)))
              (element name "" content #:new-line new-line)))))




#|
REMARK RE.SPECIFYING CLASSES

Calls to create HTML elements have variable numbers of arguments; to specify
the class attribute for the element, we cannot use a named or key parameters.
We offer a mixed approach to solve this:
* 

|#

(define (fun xs #:key key)
  (first xs))

;; HTML paragraph - simple version, no class (redundant?)
(define p
  (lambda content
    (element "p" "" content #:new-line 'after)))

(define b
  (lambda content
    (element "b" "" content  #:new-line 'none)))

(define p2
  (lambda content
    ;(printf "in p2 - content: ~a~%" content)
    (element2 "p" content #:new-line 'after)))
;(displayln (p2 "Hello " (b "World") "!" 'class "another bye"))



(define h1
  (lambda content
    (element "h1" "" content  #:new-line 'after)))

(define h2
  (lambda content
    (element "h2" "" content  #:new-line 'after)))


(define div
  (lambda content
    (element "div" "" content  #:new-line 'before-after)))

(define div2
  (lambda content
    (element2 "div" content  #:new-line 'before-after)))


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
;(displayln (p "Hello " (b "World") "!"))
;(displayln (p-c "a-class;anotherbye" "Hello " (b "World") "!"))
;(displayln (p2 "Hello " (b "World") "!" 'class "another bye"))
;(displayln (div (p "Hello " (b "World") "c")))
(displayln "--------------------")
(displayln (div2 (p2 "in a  " (b "DIV") "!" 'class "p-class")))
(displayln "--------------------")
(displayln (div2
            (div2 (p2 "now in a  " (b "NESTED DIV") "!" 'class "p-class"))
            (div2 (p2 "another paragraph in another nested div, no class"))))
(displayln "--------------------")
(displayln (p2 (b "bold first") " then normal"))
;(displayln (div-c "important"
;                  (p "Hello " (b "World") "!")
;                  (p "I am " (b "here") ".")))


             
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



(check-equal? (content->string '("a")) "a")
(check-equal? (content->string '("a" "B")) "aB")
(define test-mixed-list  '("a" (format "~a" (+ 1 2)) "B"))
(check-equal? (content->string test-mixed-list) "a3B")
