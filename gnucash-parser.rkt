#lang racket
(require
  racket/block
  racket/list
  rackunit
  racket/gui/base)

(require "gnucash-objects.rkt")

(define *TEMPLATE-ROOT-NAME* "Template Root")
(define *ROOT-NAME* "Root Account")

 ;(struct-out test-struct))

#|
Parse a GnuCash file into data structures.
Export the data structures individually instead of packaging them in a
main repo object.
|#

(define HUGE-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\michel-UNCOMPRESSED-SNAPSHOT.gnucash")
(define SMALL-SAMPLE-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\conjoint-UNCOMPRESSED-SNAPSHOT.gnucash")
(define TRUNCATED-GNUCASH-FILE
  "D:\\__DATA_FOR_APPS\\GnuCash-Uncompressed\\TRUNCATED.gnucash")

;; -------------------
;;   OBJECT MARKERS
;; -------------------
(define ACCOUNT-START "<gnc:account version=\"2.0.0\">")
(define ACCOUNT-END "</gnc:account>")
(define ACCOUNT-NAME "<act:name>")
(define ACCOUNT-TYPE "<act:type>")
(define ACCOUNT-GUID "<act:id type=\"guid\">")
(define ACCOUNT-PARENT-ID "<act:parent type=\"guid\">")

(define TRANSACTION-START "gnc:transaction version=\"2.0.0\">")
(define TRANSACTION-END "</gnc:transaction>")

;; -----------
;;   STRUCTS
;; -----------

(struct account (name type id parent-id) #:mutable)

(define (make-blank-account)
  (account "" "" "" ""))

(define (print-account act)
  (printf "~a (~a) [~a] Parent-id:~a~%"
          (account-name act) (account-type act) (account-id act) (account-parent-id act)))
  
;; print a list of command and stats about the data
#|
(define (print-commands)
   (displayln "")
   (displayln "--------")
   (displayln "COMMANDS")
   (displayln "--------")
   (displayln "(print-commands) -> display the list of available commands")
   (displayln "(print-overview) -> display an overview of the GnuCash data")
   (displayln "(print-all-accounts) -> display a list of accounts with some basic info")
   (displayln ""))
|#


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

;; read an account from the file and return in 
(define (import-account in)
  (let ([account (make-object account%)])
    ;(displayln (length *accounts*))
    (let act-loop ([line (next-line in)])
      (if (equal? line ACCOUNT-END)
          account
          (block
           (let ([value (element-value line)])
             (cond
               [(equal? line ACCOUNT-END) '()]
               [(string-prefix? line ACCOUNT-NAME)
                (send account set-name! value)]
               [(string-prefix? line ACCOUNT-PARENT-ID)
                (send account set-parent-id! value)]
               [(string-contains? line ACCOUNT-GUID)
                (send account set-id! value)]
               [(string-prefix? line ACCOUNT-TYPE)
                (send account set-type! value)]))
          (act-loop (next-line in)))))
    ;;(printf "IMPORTED account '~a'~%" (send account as-string))
    account))
   

;;-----------------------------
;;    MAIN READER AND DISPATCH
;;-----------------------------

; the GnuCash reader loops a GnuCash to the EOF and sends lines to the dispatch function
(define (import-gnucash-file path)
  (let ([gnucash (make-object gnucash-data%)])
   (send gnucash set-file-path! path)
   (call-with-input-file path    
    (lambda (in)
      (let loop ([line (next-line in)])
        (cond
          [(eof-object? line) '()]
          [ else
            (block
              (dispatch-line gnucash line in)
              (loop (next-line in)))]))))
    (printf "IMPORTED ~a ACCOUNTS before purging templates~%" (send gnucash num-accounts))
    (build-metadata gnucash)
    gnucash))

;; detect start of object definition and route to matching function
;; arg line: string current line (CR/LF and left-padding removed)
;; arg in: the input port
(define (dispatch-line gnucash-data line in)
  (cond
    [(equal? line ACCOUNT-START) (send gnucash-data add-account! (import-account in))]
    [(equal? line TRANSACTION-START) (displayln "Found a transaction")]))


;; -------------------------------------------------
;;   BUILD METADATA
;;   Links and references not explicit in raw data
;; -------------------------------------------------

;; main function for building metata
;; MODIFIES gnucash-data
;; no return(define (build-metadata gnucash-data)
(define (build-metadata gnucash-data)
  (account-metadata gnucash-data))

;; build metadata for accounts
;; eg. link to parent object using id from file,
;; build full name with all parent names,
;; hide template accounts
;; MODIFIES gnucash-date
;; no return
(define (account-metadata gnucash-data)
  (let ([accounts (send gnucash-data accounts-sorted-by-name)]
        [template-root-id (send (send gnucash-data account-by-name *TEMPLATE-ROOT-NAME*) get-id)])
    ; link to parent object
    (for ([act accounts])
      (let ([parent-id (send act get-parent-id)])
        (cond [(not (equal? "" parent-id))          
            (send act set-parent! (send gnucash-data account-by-id parent-id))])))
    ; remove templates; yes we loop again
    ; identify root
    (for ([act (send gnucash-data accounts-sorted-by-name)])
      (let ([account-name (send act get-name)]
            [account-id (send act get-id)] 
            [parent-id (send act get-parent-id)])
        ;(printf "name: ~a parent-id: ~a (t-r-id: ~a)~%" account-name parent-id template-root-id)
        (cond [(equal? account-name *ROOT-NAME*) (send gnucash-data set-root-account! act)])
        (cond [(or (equal? account-name *TEMPLATE-ROOT-NAME*) (equal? template-root-id parent-id))
              (send gnucash-data remove-account act)])))))
   


;; ----------
;;   DEMO
;; ----------

(define (demo)
  (displayln "----------------------------")
  (displayln "         DEMO               ")
  (displayln "----------------------------")
  (define gnucash-data (import-gnucash-file TRUNCATED-GNUCASH-FILE))
  (send gnucash-data print-overview)
  (displayln "----------------------------")
  (printf "parent of first account: ~a~%" (send (third (send gnucash-data accounts-sorted-by-name)) get-name))  
  (send gnucash-data display-all-accounts)
  (displayln ""))

(demo)


#|
(define main-frame (new frame% [label "My GnuCash"]
                      [width 900]
                      [height 900]
                      [alignment '(left top)]))

(send main-frame show #t)

(define group-box
  (new group-box-panel%
     (parent main-frame)
     (label "Accounts")))

(new list-box%
     [label ""]
     [parent group-box]
     [choices (hash-keys *accounts-by-name*)])
     ;[choices '("a" "k" "o")])

|#

;; --------------
;;   UNIT TESTS
;; --------------

(check-eq? (my-position "allo" #\l) 1)
(check-eq? (my-position "allo" #\x) -1)
(check-equal? (element-value "<name>bob<df") "bob")
(check-equal? (element-value "<name><df") "")
(check-equal? (element-value "name") "")
(check-equal? (element-value "name><df") "") ;; element does not start with <

