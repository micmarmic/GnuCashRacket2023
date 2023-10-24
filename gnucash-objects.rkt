#lang racket
(require rackunit)
(require racket/block)

(provide gnucash-data% account% transaction%
         print-overview display-all-accounts display-all-transactions)


;; --------------------------------------------
;; Class gnucash-data%: repository for all data
;; --------------------------------------------

;; Class gnucash-data% is repository for all data
;; Instantiated and populated by functions in gnucash-parser.rkt
(define gnucash-data%
  (class object%
    (super-new)
    (field [root-account void] [accounts-by-name (make-hash)] [accounts-by-id (make-hash)]
           [transactions-by-id (make-hash)] [file-path ""])

    ;; setters-getters
    (define/public (get-root-account) root-account)
    (define/public (set-root-account! account) (set! root-account account))
    (define/public (set-file-path! arg-path) (set! file-path arg-path))
    (define/public (get-file-path) file-path)

    (define/public (num-accounts) (length (hash-values accounts-by-id)))
    (define/public (num-transactions) (length (hash-values transactions-by-id)))

    (define/public (get-list-transactions) (hash-values transactions-by-id))
    
    (define/public (add-account! account)
      (if (void? account)
          (error "Account is void!")
          (block
           (hash-set*! accounts-by-name (send account get-name) account)
           (hash-set*! accounts-by-id (send account get-id) account))))
    
    (define/public (add-transaction! transaction)
      (if (void? transaction)
          (error "[add-transaction!] transaction is void!")
           (hash-set*! transactions-by-id (send transaction get-id) transaction)))

    (define/public (remove-account account)
      (hash-remove! accounts-by-name (send account get-name))
      (hash-remove! accounts-by-id (send account get-id)))
    
    (define/public (account-by-name name) (hash-ref accounts-by-name name))
    (define/public (account-by-id id) (hash-ref accounts-by-id id))   
    (define/public (accounts-sorted-by-name)
      (sort (hash-values accounts-by-name) account-name<?))))

;; -----------------------------------
;; Functions with gnucash-data% as arg
;; -----------------------------------
   
(define (display-all-accounts gnucash)
  (for ([act (send gnucash accounts-sorted-by-name)])
    (if (void? act)
        (display "ERROR! account is void!!!")
        (displayln (send act as-string)))))

(define (display-all-transactions gnucash)
  (for ([tran (send gnucash get-list-transactions)])
    (if (void? tran)
        (display "ERROR! transaction is void!!!")
        (displayln (send tran as-string)))))


(define (print-overview gnucash)
  (displayln "")
  (displayln "--------")
  (displayln "OVERVIEW")
  (displayln "--------")
  (printf "File path: ~a~%" (send gnucash get-file-path))
  (printf "Number of accounts: ~a~%" (send gnucash num-accounts))
  (printf "Number of transactions: ~a~%" (send gnucash num-transactions))
  (printf "(Root account has id '~a'~%" (send (send gnucash get-root-account) get-id))
  (displayln ""))

;; ------------
;; CLASS split%
;; ------------

;; A split from the GnuCash file
(define split%
  (class object%
    (super-new)
    (field [id ""] [account (void)] [value 0.0] [quantity 0.0] [memo ""] [splits '()])

    (define/public (set-id! arg-id) (set! id arg-id))
    (define/public (set-account! arg-account) (set! account arg-account))
    (define/public (set-value! arg-value) (set! value arg-value))
    (define/public (set-quantity! arg-quantity) (set! quantity arg-quantity))
    (define/public (set-memo! arg-memo) (set! memo arg-memo))

    (define/public (get-id!) id)
    (define/public (get-account) account)
    (define/public (get-value) value)
    (define/public (get-quantity) quantity)
    (define/public (get-memo) memo)
    ))

;; ------------------
;; CLASS transaction%
;; ------------------

;; An transaction from the GnuCash file and its splits
(define transaction%
  (class object%
    (super-new)
    (field [id ""] [description ""] [date-posted ""] [splits (void)])

    ;; setters-getters
    (define/public (set-id! arg-id) (set! id arg-id))
    (define/public (set-description! arg-description) (set! description arg-description))
    (define/public (set-date-posted! arg-date-posted) (set! date-posted arg-date-posted))

    (define/public (get-id) id)
    (define/public (get-description) description)
    (define/public (get-date-posted) date-posted)

    ;; add a list of splits to the transaction (one time during import)
    ;; add this transaction to the account for each individual split
    (define/public (add-all-splits! list-splits)
      (set! splits list-splits)
      (for ([split list-splits])
        (let ([split-account (send split get-account)])
          (send split-account add-transaction! this))))

    (define/public (as-string)
        (format "Date:~a Desc:~a Id: ~a" date-posted description id))  
))
    

;; ----------------
;; CLASS account%
;; ----------------

;; An account from the GnuCash file
(define account%
  (class object%
    (super-new)
    (field [name ""] [id ""] [parent-id ""] [type ""] [sort-name ""] [parent (void)]
            [full-name ""] [transactions '()])

    ;; setters-getters
    (define/public (set-name! arg-name)
      (let ([arg-sort-name (lcase-no-accents arg-name)])
        (set! name arg-name)
        (set! sort-name arg-sort-name)))
    (define/public (get-name) name)

    (define/public (set-full-name! arg-full-name) (set! full-name arg-full-name))
    (define/public (get-full-name) full-name)
    
    (define/public (get-sort-name) sort-name)
    (define/public (set-id! arg-id) (set! id arg-id))
    (define/public (get-id) id)
    (define/public (set-parent-id! arg-parent-id) (set! parent-id arg-parent-id))
    (define/public (get-parent-id) parent-id)
    (define/public (set-parent! arg-parent) (set! parent arg-parent))
    (define/public (get-parent) parent)
    (define/public (set-type! arg-type) (set! type arg-type))
    (define/public (get-type) type)
   
    (define/public (add-transaction! transaction) (set! transactions (cons transaction transactions)))
    
    ;; display
    (define/public (as-string)
      (let ([parent-name "TODO"])
        (if (void? parent)
            (set! parent-name "NO PARENT")
            (set! parent-name (send parent get-name)))        
        (format "~a (~a) [id: ~a] [parent-name: ~a] Full name: '~a'" name type id parent-name full-name)))  
))


;;-----------------------
;;  HELPERS FOR SORTING
;;-----------------------

;; list of chars to replace in lcase-no-accents
(define ACCENTED "àâäçéèêëìîïòôöùûü")
(define ACCENT-REMOVED "aaaceeeeiiiooouuu")

(define (compare-strings-without-accents a b)
  (let ([a-fixed (lcase-no-accents a)]
        [v-fixed (lcase-no-accents b)])
    (string<? a b)))
        
(define (account-name<? a1 a2)
  (string<? (send a1 get-sort-name) (send a2 get-sort-name)))

;; replace accented chars with non-accented equiv and lower the case
(define (lcase-no-accents str)
  (if (equal? str "")
      ""
      (let ([newstr (string-downcase str)])
        (for ([i (in-range 0 (string-length ACCENTED))])
          (set! newstr (string-replace newstr
                                       (~a (string-ref ACCENTED i))
                                       (~a (string-ref ACCENT-REMOVED i)))))
        newstr)))
  


;; DEBUGGING CODE
#|
(define myrepo (make-object gnucash-data%))
(define a1 (make-object account%))
(send a1 set-name! "Aleve")
(define a2 (make-object account%))
(send a2 set-name! "Brie")
(define a3 (make-object account%))
(send a3 set-name! "Fory")
(define a4 (make-object account%))
(send a4 set-name! "Étude")

(send myrepo add-account a1)
(send myrepo add-account a2)
(send myrepo add-account a3)
(send myrepo add-account a4)
(send myrepo display-all-accounts)
;(printf "num accounts: ~a~%" (send myrepo num-accounts))
|#

;; -----------
;; UNIT TEST
;; -----------

(check-equal? (lcase-no-accents ACCENTED) ACCENT-REMOVED)
(check-equal? (string-length ACCENTED) (string-length ACCENT-REMOVED))

(check-equal? (lcase-no-accents "béb") "beb")
(check-equal? (lcase-no-accents "Michel rembourse dépenses") "michel rembourse depenses")