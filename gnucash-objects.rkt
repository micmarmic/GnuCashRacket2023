#lang racket
(require rackunit)
(require racket/block)

(provide gnucash-data% account% )

(define gnucash-data%
  (class object%
    (super-new)
    (field [root-account void] [accounts-by-name (make-hash)] [accounts-by-id (make-hash)]
                [file-path ""])

    ;; setters-getters
    (define/public (set-root-account! account) (set! root-account account))
    (define/public (set-file-path! arg-path) (set! file-path arg-path))
    (define/public (get-file-path) file-path)
    (define/public (num-accounts) (length (hash-values accounts-by-id)))
    
    (define/public (add-account! account)
      (if (void? account)
          (error "Account is void!")
          (block
           (hash-set*! accounts-by-name (send account get-name) account)
           (hash-set*! accounts-by-id (send account get-id) account))))
    (define/public (remove-account account)
      (hash-remove! accounts-by-name (send account get-name))
      (hash-remove! accounts-by-id (send account get-id)))
    
    (define/public (account-by-name name) (hash-ref accounts-by-name name))
    (define/public (account-by-id id) (hash-ref accounts-by-id id))   
    (define/public (accounts-sorted-by-name)
      (sort (hash-values accounts-by-name) account-name<?))

    

    ;; misc methods
    (define/public (display-all-accounts)
      (for ([act (accounts-sorted-by-name)])
        (if (void? act)
            (display "ERROR! account is void!!!")
            (displayln (send act as-string)))))

    (define/public (print-overview)
      (displayln "")
      (displayln "--------")
      (displayln "OVERVIEW")
      (displayln "--------")
      (printf "File path: ~a~%" file-path)
      (printf "Number of accounts: ~a~%" (num-accounts))
      (printf "(Root account has id '~a'~%" (send root-account get-id))
      (displayln ""))

))

    
(define account%
  (class object%
    (super-new)
    (field [name ""] [id ""] [parent-id ""] [type ""] [sort-name ""] [parent void])

    ;; setters-getters
    (define/public (set-name! arg-name)
      (let ([arg-sort-name (lcase-no-accents arg-name)])
        (set! name arg-name)
        (set! sort-name arg-sort-name)))
    (define/public (get-name) name)
    (define/public (get-sort-name) sort-name)
    (define/public (set-id! arg-id) (set! id arg-id))
    (define/public (get-id) id)
    (define/public (set-parent-id! arg-parent-id) (set! parent-id arg-parent-id))
    (define/public (get-parent-id) parent-id)
    (define/public (set-parent! arg-parent) (set! parent arg-parent))
    (define/public (get-parent) parent)
    (define/public (set-type! arg-type) (set! type arg-type))
    (define/public (get-type) type)
   

    ;; display
    (define/public (as-string)
      (let ([parent-name "TODO"])
        (if (equal? "" parent-id)
            (set! parent-name "NO PARENT")
            (set! parent-name (send parent get-name)))        
        (format "~a (~a) [id: ~a] [parent-name: ~a]" name type id parent-name)))  
))


;;-----------
;;  HELPERS
;;-----------

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
(check-equal? (lcase-no-accents ACCENTED) ACCENT-REMOVED)
(check-equal? (string-length ACCENTED) (string-length ACCENT-REMOVED))

(check-equal? (lcase-no-accents "béb") "beb")
(check-equal? (lcase-no-accents "Michel rembourse dépenses") "michel rembourse depenses")