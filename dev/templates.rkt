#lang racket
(require rackunit)

(provide render-template filepath->string)

#|
The template.rkt provides functions to support working with tempaltes.

(render-template filepath hash-key-value-pairs) replaces %markers% with values.
render-template doesn't currently support any other functionality.

filepath->string can be used to load a css file or javascript file.
|#

;; given the path to a template and a dict of tag - values
;; replace the tags in the template with the values
;; path: string full path to file
;; tags: list of pairs or string tag and string values
;; example: ("title" "Home page" "css" "/css/main.css")
(define (render-template path tags)
  (let ([template (filepath->string path)])
    (replace-tags-with-values template tags)))

;; load file into a string
(define (filepath->string path)
   (call-with-input-file path    
    (lambda (in)
      (let loop ([line (read-line in 'any)]
                 [file-contents ""])
        (cond
          [(eof-object? line) file-contents]
          [ else
              (loop (read-line in 'any) (string-append file-contents line "\n"))])))))

;; given a string, replace all %markers% with the matching value in the tags-values hash or pairs
;; in the values of the pairs, just specify the marker name
;; in the input, surround the marker with %
;; input: "My name is %name%"; tags-values: (("name" . "Bob")); result: "My name is Bob"
(define (replace-tags-with-values input tags-values)
  (let* ([result input])
    (hash-for-each tags-values
                   (lambda (key value)
                     (let ([marker (format "%~a%" key)])
                       (if (not (string-contains? input marker))
                           (error (format "error: marker ­~a not found in template" marker))
                           (set! result (string-replace result marker value))))))
    result))
                
;; ----------
;; UNIT TESTS
;; ----------
;;(define test-directory "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\tests\\")
(define tests-directory (build-path (current-directory) "tests\\"))
(define test-template-path-1 (build-path tests-directory "test-template-1.html"))
(check-equal? (render-template test-template-path-1 #hash(("title" . "Hello Title") ("header" . "This is the Header")))
              "<title>Hello Title</title>\n<h1>This is the Header</h1>\n")
(check-equal? (replace-tags-with-values "%title% %body%" #hash(("title" . "YES title") ("body" . "YES body"))) "YES title YES body" )

(printf "Current directory: ~a~%" (current-directory))
(printf "Tests directory: ~a~%" tests-directory)