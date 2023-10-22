#lang racket
(require racket/block)
(require racket/list)
(require rackunit)
(require compatibility/mlist)

#|
Parse a GnuCash file into structs or classes.

We will parse the XML-like line by line instead of using an XML parser.

Object definitions start and end with specific "markers". For example, an account
definition starts with: <gnc:account version="2.0.0"> and ends with: </gnc:account>.

|#

(define HUGE-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define SMALL-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")
(define TRUNCATED-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\TRUNCATED.gnucash")

;; -------------------
;;   OBJECTS MARKERS
;; -------------------
(define ACCOUNT-START "<gnc:account version=\"2.0.0\">")
(define ACCOUNT-END "</gnc:account>")
(define ACCOUNT-NAME "<act:name>")
(define ACCOUNT-TYPE "<act:type>")
(define ACCOUNT-GUID "<act:id type=\"guid\">")


(define TRANSACTION-START "gnc:transaction version=\"2.0.0\">")
(define TRANSACTION-END "</gnc:transaction>")

(define debug-count-account 0)
(define debug-count-transactions 0)

;; -----------
;;   STRUCTS
;; -----------

(struct account (name type id) #:mutable)

(define (make-blank-account)
  (account "" "" ""))

(define (print-account act)
  (block
   (display (account-name act))
   (display " (")
   (display (account-type act))
   (displayln ")")))

;; ----------------
;;   GLOBAL LISTS

;;  lists are mutable so simplify the import process
;; 
;; ----------------
;; TODO: eliminate globals?
(define *accounts* (list))

(define (print-all-accounts)
  (for ([act *accounts*])
    (print-account act)))

;;-----------
;;  HELPERS
;;-----------

;; given a port, return the next-line trimmed
(define (next-line in)
  (let ([line (read-line in 'any)])
    (if (eof-object? line)
        line
        (string-trim line))))

;; return the location of a character in a string, -1 if not found
;; char must be passed as a character, like "a" is #\a
(define (my-position str char)
  (let ([len (string-length str)])
    (let loop ([n 0])
      (cond
         [(equal? n len) -1]
         [(equal? char (string-ref str n)) n]
         [else (loop (add1 n))]))))

;; extract inner string from xml-like element
(define (element-value str)
  (let ([index> (my-position str #\>)])
    (if (negative? index>)
        ""
        ;; skip the first < and add 1 to index later
        (let ([index< (my-position (substring str 1) #\<)])
          (if (negative? index<)
              ""
              (substring str (+ 1 index>) (add1 index<)))))))

;;-------------------
;; FACTORY FUNCTIONS
;;------------------

#|
(define (import-account in)
  (let ([account '()])
    (let act-loop ([line (next-line in)])
      (cond
        [(equal? line ACCOUNT-END) (displayln account)]
        [(string-prefix? line ACCOUNT-NAME)
         (block
          (set! account (element-value line))
          (act-loop (next-line in)))]
       [(string-contains? line ACCOUNT-GUID)
                 (block
          (set! account (cons (element-value line) account))
          (act-loop (next-line in)))]
       [(string-prefix? line ACCOUNT-TYPE)
         (block
          (set! account (cons (element-value line) account))
          (act-loop (next-line in)))]
        [else (act-loop (next-line in))]))
    (displayln account)))
|#

(define (import-account in)
  (let ([account (make-blank-account)])
    (set! *accounts* (cons account *accounts*))
    ;(displayln (length *accounts*))
    (let act-loop ([line (next-line in)])
      (if (equal? line ACCOUNT-END)
          account
          (block
           (let ([value (element-value line)])
             (cond
               [(equal? line ACCOUNT-END) '()]
               [(string-prefix? line ACCOUNT-NAME)
                (set-account-name! account value)]
               [(string-contains? line ACCOUNT-GUID)
                (set-account-id! account value)]
               [(string-prefix? line ACCOUNT-TYPE)
                (set-account-type! account value)]))
          (act-loop (next-line in)))))    
    account))
   

;;-----------------------------
;;    MAIN READER AND DISPATCH
;;-----------------------------

; the GnuCash reader loops a GnuCash to the EOF and sends lines to the dispatch function
(define (import-gnucash-file path)
  (block
   (call-with-input-file path    
    (lambda (in)
      (let loop ([line (next-line in)])
        (cond
          [(eof-object? line) '()]
          [ else
            (block
              (dispatch-line line in)
              (loop (next-line in)))]))))
   (displayln "IMPORT COMPLETED")))
;;      (displayln (read-line in)))))

;; detect start of object definition and route to matching function
;; arg line: string current line (CR/LF and left-padding removed)
;; arg in: the input port
(define (dispatch-line line in)
  (cond
    [(equal? line ACCOUNT-START) (import-account in)]
    [(equal? line TRANSACTION-START) (displayln "Found a transaction")]))
      
;; ----------
;;    DEMOS
;; ----------

(define (demo)
  (import-gnucash-file TRUNCATED-GNUCASH-FILE))

(demo)
(print-all-accounts)
  

;; --------------
;;   UNIT TESTS
;; --------------

(check-eq? (my-position "allo" #\l) 1)
(check-eq? (my-position "allo" #\x) -1)
(check-equal? (element-value "<name>bob<df") "bob")
(check-equal? (element-value "<name><df") "")
(check-equal? (element-value "name") "")
(check-equal? (element-value "name><df") "") ;; element does not start with <


