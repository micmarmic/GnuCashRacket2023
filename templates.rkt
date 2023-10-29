#lang racket
(require racket/block) ; for testing - remove

(provide render-template)

#|
The template.rkt emulates the functions in various web
framework libraries
|#

;; given the path to a template and a dict of tag - values
;; replace the tags in the template with the values
;; path: string full path to file
;; tags: list of pairs or string tag and string values
;; example: ("title" "Home page" "css" "/css/main.css")
(define (render-template path tags)
  (displayln "allo from render-template")
  (for ([key (hash-keys tags)])
    (let ([value (hash-ref tags key)])
      (printf "Key: ~a Value: ­~a~%" key value)))
  (let ([template (read-template-file path)])
    (displayln "Template")
    (replace-tags-with-values template tags)))


;; return file contents as string
;(define (read-file path) (format "Template will be read from '~a'~%" path))
(define (read-template-file path)
   (call-with-input-file path    
    (lambda (in)
      (let loop ([line (read-line in 'any)]
                 [file-contents ""])
        (cond
          [(eof-object? line) file-contents]
          [ else
            (block
              (display line)
              (loop (read-line in 'any) (string-append file-contents line "\n")))])
        ))))

(define (replace-tags-with-values input tags-values)
  (let* ([result input])
    (hash-for-each tags-values
                   (lambda (key value)
                     (let ([marker (format "%~a%" key)])
                       (if (not (string-contains? input marker))
                           (error (format "error: marker ­~a not found in template" marker))
                           (set! result (string-replace result marker value))))))
    result))
                

;; ----------------------
;; DEMO (uncomment lines)
;; ----------------------
(define TEMPLATES-DIRECTORY "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\templates\\")
(define test-template-path-1 (string-append TEMPLATES-DIRECTORY "test-template1.html"))
test-template-path-1
    
(render-template test-template-path-1 #hash(("title" . "Home") ("header" . "This is the Header")))
(replace-tags-with-values "%title% %body%" #hash(("title" . "YES title") ("body" . "YES body")))