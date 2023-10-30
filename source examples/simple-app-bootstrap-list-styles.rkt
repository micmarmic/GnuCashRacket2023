#lang racket
(require web-server/servlet)  ; Provides dispatch-rules.
; Provides serve/servlet and happens to provide response/full.
(require web-server/servlet-env)

(define (http-response content)  ; The 'content' parameter should be a string.
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))

;; ----------------------------
;; DUPLICATED FROM template.rkt

;; load file into a string
(define (filepath->string path)
   (call-with-input-file path #:mode 'text   
    (lambda (in)
      (let loop ([line (read-line in 'any)]
                 [file-contents ""])
        (cond
          [(eof-object? line) file-contents]
          [ else
              (loop (read-line in 'any) (string-append file-contents line "\n"))])))))

;; load image and return
(define (send-favico request)
  (let* ([path "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\static\\images\\favicon.ico"]
        [favico-bytes (load-favico path)])
    (printf "****** favico-bytes is bytes?: ~a~%" (bytes? favico-bytes))
    (http-response-favico favico-bytes)))
          

(define (load-favico path)
  (file->bytes path))
 #|
   (call-with-input-file path #:mode 'binary
    (lambda (in)
      (read in))))
|#

  
(define (http-response-favico arg-bytes)  ; The 'content' parameter should be a string.
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    #"image/x-icon"  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list arg-bytes)))                ; Content (in bytes) to send to the browser.
      

;; -------------------------

(define (list-demo-html)
  (filepath->string "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\html-samples\\list-styles-bootstrap.html"))


(define (show-lists-demo request)
  (http-response (list-demo-html)))

;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
               (dispatch-rules
                 [("lists") show-lists-demo]
                 [("favicon.ico") send-favico]
                 [else (error "There is no procedure to handle the url.")]))

(define (request-handler request)
  (printf "Request path: ~a~%" (url->string (request-uri request)))
  (dispatch request))

(define (my-start-server)
;; Start the server.
  (serve/servlet
   request-handler
   #:launch-browser? #f
   #:quit? #f
   #:listen-ip "127.0.0.1"
   #:port 8000
   #:servlet-regexp #rx""))

(my-start-server)