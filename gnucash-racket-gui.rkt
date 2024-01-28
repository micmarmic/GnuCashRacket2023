#lang racket

#|
This file: gnucash-racket-gui.rkt

Purpose:   Define the GUI to access GnuCash data from a racket app.

Run:       Run this file to execute (main)

Main runs in two phases:

1) Configure: read settings and load data; display GUI window with error message(s)
              in case of failure.

2) Display views: load the main GUI; uses menus/buttons to load other views.
|#

(require (file "lib/allocation.rkt")
         (file "lib/finance.rkt")
         (file "lib/gnucash-parser.rkt")
         (file "lib/gnucash-objects.rkt")
         (file "views/gui-views.rkt")
         (file "views/main-gui.rkt")
         (file "views/web-views.rkt"))

(define %global-gnucash-data% null) ; global set in (start-app)
(define %global-allocation-data% null) ; global set in (start-app)
(define DEFAULT-ALLOCATION-DATA-FILE "allocation-data.txt")
(define DEFAULT-PATH-DATA-FILE "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
;(define DEFAULT-PATH-DATA-FILE "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")
;(define DEFAULT-PATH-DATA-FILE "d:\\Documents\\programming\\racket\\racket-projects\\GnuCash\\tests\\test-file1.gnucash")

(define (main)

  (define error-message "")
  (with-handlers
      ([exn:fail?
        (λ (e)
          (set! error-message
                (format "ERROR STARTING WEB APP: ~a" (exn-message e))))])
    
    (displayln "Loading configuration.")    
    (set! %global-allocation-data% (file->alloc-hash DEFAULT-ALLOCATION-DATA-FILE))
    (displayln "Configuration okay.")    
    (displayln "")  
    
    (printf "Loading data file '~a'~%" DEFAULT-PATH-DATA-FILE) 
    (set! %global-gnucash-data% (import-gnucash-file DEFAULT-PATH-DATA-FILE))  
    (printf "Loading data complete.~%")
    (displayln "")  
    (print-overview %global-gnucash-data%)
    (displayln "")

    (define hash-data (get-roi-table-data %global-gnucash-data% "2023-12-31" %global-allocation-data%))
    (displayln "Loading complete")
  
    (displayln "DEMO")
    (for ([key (hash-keys hash-data)])
      (printf "Account: ~a~%" key)
      (displayln (hash-ref hash-data key)))
    (displayln "")
    (displayln "END DEMO")
    )
  (if (equal? "" error-message)
      (main-gui %global-gnucash-data% %global-allocation-data%)
      (display-cannot-start-gui error-message)        
      )
)



(main)