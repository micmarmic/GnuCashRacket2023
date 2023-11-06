#lang racket

(provide import-gnucash-file)

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

(define TRANSACTION-START "<gnc:transaction version=\"2.0.0\">")
(define TRANSACTION-END "</gnc:transaction>")
(define TRANSACTION-DESCRIPTION "<trn:description>")
(define TRANSACTION-DATE-POSTED "<trn:date-posted>")
(define TRANSACTION-ID "<trn:id type=\"guid\">")

(define ALL-SPLITS-START "<trn:splits>")
(define ALL-SPLITS-END "</trn:splits>")
(define SPLIT-START "<trn:split>")
(define SPLIT-END "</trn:split>")
(define SPLIT-ID "<split:id type=\"guid\">")
(define SPLIT-MEMO "<split:memo>")
(define SPLIT-VALUE "<split:value>")
(define SPLIT-QUANTITY "<split:quantity>")
(define SPLIT-ACCOUNT-ID "<split:account type=\"guid\">")

(define COMMODITY-START "<gnc:commodity version=\"2.0.0\">")
(define COMMODITY-END "</gnc:commodity>")
(define COMMODITY-ID "<cmdty:id>")
(define COMMODITY-NAME "<cmdty:name>")
(define COMMODITY-FRACTION "<cmdty:fraction>")
(define COMMODITY-SPACE "<cmdty:space>")


;;------------------
;;  IMPORT HELPERS
;;------------------
 
;; given a port, return the next-line, trimmed
;; NOTE: some account description span many lines;
;;       one may need to fix that here (loop and append until line ends in >)
(define (next-line in)
  (let ([line (read-line in 'any)])
    (if (eof-object? line)
        line
        (string-trim line))))

;; return the location of a character in a string, -1 if not found
;; char must be passed as a character, like "a" is #\a
(define (char-position str char)
  (let ([len (string-length str)])
    (let loop ([n 0])
      (cond
         [(equal? n len) -1]
         [(equal? char (string-ref str n)) n]
         [else (loop (add1 n))]))))

;; extract inner string from xml-like element
;; fix: some values in account description can be multiline
;;      so, some lines don't have a start and finish
;;      FIX is skip lines that don't start with '<'
(define (element-value str)
  (if (not (string-prefix? str "<"))
      ""
      (let ([index> (char-position str #\>)])
        (if (negative? index>)
            ""
            ;; skip the first < and add 1 to index later
            (let ([index< (char-position (substring str 1) #\<)])
              (if (negative? index<)
                  ""
                  (substring str (+ 1 index>) (add1 index<))))))))

;; the required values is on the next line; skip one line after
;; example:
;;   <date>
;;     <iso-date>2012-11-12</iso-date>
;;   </date>
;; arg in is what is needed; don't care about the value of the starting line like <date>
(define (next-line-element-value in)
  (let ([line (next-line in)])
    (element-value line)))
  
;;-------------------
;; IMPORT FUNCTIONS
;;------------------

;; read a commodity from the file and return it
(define (import-commodity in)
  (let ([commodity (make-object commodity%)])
    ;(displayln (length *accounts*))
    (let act-loop ([line (next-line in)])
      (if (equal? line COMMODITY-END)
          commodity
          (block
           (let ([value (element-value line)])
             (cond
               [(string-prefix? line COMMODITY-NAME)
                (send commodity set-name! value)]
               [(string-prefix? line COMMODITY-ID)
                (send commodity set-id! value)]
               [(string-contains? line COMMODITY-FRACTION)
                (send commodity set-fraction! value)]
               [(string-prefix? line COMMODITY-SPACE)
                (send commodity set-space! value)]))
          (act-loop (next-line in)))))    
    commodity))

;; read an account from the file and return it
(define (import-account in)
  (let ([account (make-object account%)])
    ;(displayln (length *accounts*))
    (let act-loop ([line (next-line in)])
      (if (equal? line ACCOUNT-END)
          account
          (block
           (let ([value (element-value line)])
             (cond
               [(string-prefix? line ACCOUNT-NAME)
                (send account set-name! value)]
               [(string-prefix? line ACCOUNT-PARENT-ID)
                (send account set-parent-id! value)]
               [(string-contains? line ACCOUNT-GUID)
                (send account set-id! value)]
               [(string-prefix? line ACCOUNT-TYPE)
                (send account set-type! value)]))
          (act-loop (next-line in)))))    
    account))

;; read a single split from the file and return it
(define (import-single-split in)
  (let ([split (make-object split%)])
    (let split-loop ([line (next-line in)])
      (if (equal? line SPLIT-END)
          split
          (block
           (let ([value (element-value line)])
             (cond
               [(string-prefix? line SPLIT-ID)
                (send split set-id! value)]
               [(string-prefix? line SPLIT-VALUE)
                (send split set-value! (string->number value))]
               [(string-contains? line SPLIT-QUANTITY)
                 (send split set-quantity! (string->number value))]
               [(string-contains? line SPLIT-MEMO)
                 (send split set-memo! value)]
               [(string-prefix? line SPLIT-ACCOUNT-ID)
                (send split set-account-id! value)]))
           (split-loop (next-line in)))))
    ;;(printf "IMPORTED split:: ~a~%" (send split as-string))
    split))

;; import individual splits from split section in file
(define (import-all-splits in)
  ;; on entry, we just read the start of all splits 
  ;; the next line is the start of single split, and we'll
  ;; be handled/ignored by the import-single-split function
  (let ([splits '()])
    (let split-loop ([line (next-line in)])
      (if (equal? line ALL-SPLITS-END)
          splits
          (block
           ;(displayln "--- IMPORT A SPLIT:")
           (set! splits (cons (import-single-split in) splits))
           (split-loop (next-line in)))))
    splits))
    

;; read a transaction from the file and return it
(define (import-transaction in)
  (let ([transaction (make-object transaction%)])
    (let tran-loop ([line (next-line in)])
      (if (equal? line TRANSACTION-END)
          transaction
          (block
           (let ([value (element-value line)])
             (cond
               [(string-prefix? line TRANSACTION-DESCRIPTION)
                (send transaction set-description! value)]
               [(string-prefix? line TRANSACTION-DATE-POSTED)
                (send transaction set-date-posted!
                      (substring (next-line-element-value in) 0 10))]
               [(string-contains? line TRANSACTION-ID)
                 (send transaction set-id! value)]
               [(string-prefix? line ALL-SPLITS-START)
                 (send transaction set-splits! (import-all-splits in))]))
           (tran-loop (next-line in)))))
    ;(printf "IMPORTED transaction ~a~%" (send transaction get-id))
    transaction))

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
    ;(printf "IMPORTED ~a ACCOUNTS before purging templates~%" (send gnucash num-accounts))
    (build-metadata gnucash)
    gnucash))

;; detect start of object definition and route to matching function
;; arg line: string current line (CR/LF and left-padding removed)
;; arg in: the input port
(define (dispatch-line gnucash-data line in)
  (cond
    [(equal? line ACCOUNT-START) (send gnucash-data add-account! (import-account in))]
    [(equal? line COMMODITY-START) (send gnucash-data add-commodity! (import-commodity in))]
    [(equal? line TRANSACTION-START) (send gnucash-data add-transaction! (import-transaction in))]))


;; -------------------------------------------------
;;   BUILD METADATA
;;   Links and references not explicit in raw data
;; -------------------------------------------------

;; main function for building metadata then clearing global transaction list
;; MODIFIES gnucash-data
;; no return(define (build-metadata gnucash-data)
(define (build-metadata gnucash-data)
  (account-metadata gnucash-data)
  (link-split-account-transaction gnucash-data)
  (send gnucash-data clear-all-transactions))


;; add matching transaction to account, account to split (not just id)
;; template transactions exit for accounts that were deleted - skip them
(define (link-split-account-transaction gnucash-data)
  (for ([transaction (send gnucash-data get-list-transactions)])
    (for ([split (send transaction get-splits)])
      (let* ([account-id (send split get-account-id)]
             [account (send gnucash-data account-by-id account-id)])
        (cond [(not (void? account))
               (block
                (send split set-account! account)
                (send account add-transaction! transaction))])))))
             

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
    ; build full name with recursive function
    (for ([act (send gnucash-data accounts-sorted-by-name)])
      (let ([account-name (send act get-name)]
            [account-id (send act get-id)] 
            [parent-id (send act get-parent-id)])
        ;(printf "name: ~a parent-id: ~a (t-r-id: ~a)~%" account-name parent-id template-root-id)
        
        (cond [(equal? account-name *ROOT-NAME*) (send gnucash-data set-root-account! act)]
              [(or (equal? account-name *TEMPLATE-ROOT-NAME*) (equal? template-root-id parent-id))
              (send gnucash-data remove-account act)]
              [else (build-full-name act)])))))

;; prepend the parent name to the full-name of the account
;; recurse up the parent tree
(define (build-full-name account)
  ; start by setting full-name to account name
  (send account set-full-name! (send account get-name))
  (let loop ([parent (send account get-parent)])
    (cond
         [(not (or (void? parent) (equal? *ROOT-NAME* (send parent get-name))))
               (let* ([full-name (send account get-full-name)]
                      [parent-name (send parent get-name)]
                      [new-full-name (string-append parent-name ":" full-name)])
                 (send account set-full-name! new-full-name)
                 (loop (send parent get-parent)))])))

  #|
  (if (void? current-parent)
      void
      (let* ([parent-name (send current-parent get-name)]
            [full-name (send account get-full-name)]
            [new-full-name (string-append parent-name ":" full-name)])
        (send account set-full-name! new-full-name)
        (build-full-name-rec account (send current-parent get-parent)))))
               
  |#

;; ----------
;;   DEMO
;; ----------


(define (display-all-trans-for-bmo-mastercard gnucash-data)
    (let ([account (send gnucash-data account-by-name "BMO Mastercard")])
      (displayln "ALL TRANSACTIONS IN 'BMO Mastercard'")
      (displayln "------------------------------------")
      (display-all-transactions-all-splits-in-account account)))


(define (demo)
  (displayln "----------------------------")
  (displayln "         DEMO               ")
  (displayln "----------------------------")
  (define gnucash-data (import-gnucash-file HUGE-SAMPLE-GNUCASH-FILE))
  ;(define gnucash-data (import-gnucash-file SMALL-SAMPLE-GNUCASH-FILE))
  (print-overview gnucash-data)
  (displayln "----------------------------")
  ;(display-all-accounts gnucash-data )
  (define first-tran (first (send gnucash-data get-list-transactions)))
  (displayln first-tran)
  ;(displayln (send first-tran as-string-single-line))
  ;(let ([tran (first (send gnucash-data get-list-transactions))])
  ;  (displayln (send tran as-string)))
  ;(displayln (send  as-string))
  ;(display-all-transactions gnucash-data)
  (display-all-commodities gnucash-data)
  ;(display-all-trans-for-bmo-mastercard gnucash-data)
  (displayln ""))
; (demo)


;; --------------
;;   UNIT TESTS
;; --------------

#|
(check-eq? (char-position "allo" #\l) 1)
(check-eq? (char-position "allo" #\x) -1)
(check-equal? (element-value "<name>bob<df") "bob")
(check-equal? (element-value "<name><df") "")
(check-equal? (element-value "name") "")
(check-equal? (element-value "name><df") "") ;; element does not start with <
(check-equal? (element-value "<name>asdfasdfa") "") ;; element doesn't end on same line: skip it
(check-equal? (element-value "asdfafda</name>") "") ;; element ends here, but we don't have the start, so skip it
|#