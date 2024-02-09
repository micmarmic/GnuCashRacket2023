#lang racket
(require web-server/servlet)  ; Provides dispatch-rules.
; Provides serve/servlet and happens to provide response/full.
(require web-server/servlet-env
         web-server/templates         
         "views.rkt")

(define %base-template-path% "templates/base-template.html")

(define (demo)
  (let* ([view-heading (format "Asset Allocation")]
         [page-title (format "~a | GnuCash" view-heading)]
         [main-content-heading view-heading])
      (response-200-base-template page-title main-content-heading
                                    (include-template "templates/allocation.html"))))



(define-values (dispatch generate-url)
  (dispatch-rules
   [("") (lambda (request) (demo))]
   [else generic-404]))

(define (request-handler request)
  (printf "Request path: ~a~%" (url->string (request-uri request)))
  (dispatch request))

(define (start-app)
  ;; load the data
  (printf "Launching the web server.~%")
  ;; Start the server.
  (serve/servlet
   request-handler
   #:launch-browser? #t
   #:servlet-path "/"
   #:quit? #f
   #:listen-ip "127.0.0.1"
   #:port 8000
   #:servlet-regexp #rx""))


(start-app)